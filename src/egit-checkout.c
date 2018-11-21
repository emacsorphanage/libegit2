#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-checkout.h"


// =============================================================================
// Helpers - Checkout Options

typedef struct {
    emacs_env *env;
    emacs_value callback;
} checkout_options_ctx;

static int checkout_notify_callback(
    git_checkout_notify_t why,
    const char *path,
    __attribute__((unused)) const git_diff_file *baseline,
    __attribute__((unused)) const git_diff_file *target,
    __attribute__((unused)) const git_diff_file *workdir,
    void *payload)
{
    checkout_options_ctx *ctx = (checkout_options_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[2];
    args[0] = em_nil;
    args[1] = EM_STRING(path);

    switch (why) {
    case GIT_CHECKOUT_NOTIFY_IGNORED:
        args[0] = em_ignored;
        break;
    case GIT_CHECKOUT_NOTIFY_UNTRACKED:
        args[0] = em_untracked;
        break;
    case GIT_CHECKOUT_NOTIFY_UPDATED:
        args[0] = em_updated;
        break;
    case GIT_CHECKOUT_NOTIFY_DIRTY:
        args[0] = em_dirty;
        break;
    case GIT_CHECKOUT_NOTIFY_CONFLICT:
        args[0] = em_conflict;
        break;
    default: break;
    }

    emacs_value retval = env->funcall(env, ctx->callback, 2, args);
    EM_RETURN_IF_NLE(GIT_EUSER);

    if (EM_EQ(retval, em_abort))
        return GIT_EUSER;
    return 0;
}

static void checkout_progress_callback(
    const char *path,
    size_t completed_steps,
    size_t total_steps,
    void *payload)
{
    checkout_options_ctx *ctx = (checkout_options_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[3];
    args[0] = path ? EM_STRING(path) : em_nil;
    args[1] = EM_INTEGER(completed_steps);
    args[2] = EM_INTEGER(total_steps);

    env->funcall(env, ctx->callback, 3, args);

    // TODO: Since we can't abort from inside a progress callback, we
    // clear non-local exits, essentially running the whole callback
    // in an implicit (ignore-errors ...) form.
    if (env->non_local_exit_check(env))
        env->non_local_exit_clear(env);
}

static emacs_value checkout_options_parse(emacs_env *env, emacs_value alist, git_checkout_options *opts)
{
    git_checkout_init_options(opts, GIT_CHECKOUT_OPTIONS_VERSION);

    emacs_value notify_callback = em_nil;
    emacs_value progress_callback = em_nil;

    // Main loop through the options alist
    {
        emacs_value car, cdr;
        EM_DOLIST(option, alist, options);
        EM_ASSERT_CONS(option);

        car = em_car(env, option);
        cdr = em_cdr(env, option);

        // TODO: Support the whole range of checkout strategies and options
        if (EM_EQ(car, em_strategy)) {
            if (!EM_EXTRACT_BOOLEAN(cdr) || EM_EQ(cdr, em_none))
                opts->checkout_strategy = GIT_CHECKOUT_NONE;
            else if (EM_EQ(cdr, em_safe))
                opts->checkout_strategy = GIT_CHECKOUT_SAFE;
            else if (EM_EQ(cdr, em_force))
                opts->checkout_strategy = GIT_CHECKOUT_FORCE;
        }
        else if (EM_EQ(car, em_notify_when)) {
            if (EM_EQ(cdr, em_all))
                opts->notify_flags |= GIT_CHECKOUT_NOTIFY_ALL;
            else {
                EM_DOLIST(icar, cdr, notify_when);
                if (EM_EQ(icar, em_conflict))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_CONFLICT;
                else if (EM_EQ(icar, em_dirty))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_DIRTY;
                else if (EM_EQ(icar, em_updated))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_UPDATED;
                else if (EM_EQ(icar, em_untracked))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_UNTRACKED;
                else if (EM_EQ(icar, em_ignored))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_IGNORED;
                else if (EM_EQ(icar, em_all))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_ALL;
                EM_DOLIST_END(notify_when);
            }
        }
        else if (EM_EQ(car, em_notify)) {
            EM_ASSERT_FUNCTION(cdr);
            notify_callback = cdr;
        }
        else if (EM_EQ(car, em_progress)) {
            EM_ASSERT_FUNCTION(cdr);
            progress_callback = cdr;
        }
        else if (EM_EQ(car, em_baseline)) {
            egit_type type = egit_get_type(env, cdr);
            if (type == EGIT_TREE)
                opts->baseline = EGIT_EXTRACT(cdr);
            else if (type == EGIT_INDEX)
                opts->baseline_index = EGIT_EXTRACT(cdr);
            else {
                // TODO: libgit-tree-or-index-p?
                em_signal_wrong_type(env, em_libgit_tree_p, cdr);
                return em_nil;
            }
        }

        EM_DOLIST_END(options);
    }

    if (EM_EXTRACT_BOOLEAN(notify_callback)) {
        checkout_options_ctx *ctx = (checkout_options_ctx*) malloc(sizeof(checkout_options_ctx));
        ctx->env = env;
        ctx->callback = notify_callback;
        opts->notify_payload = ctx;
        opts->notify_cb = &checkout_notify_callback;
    }

    if (EM_EXTRACT_BOOLEAN(progress_callback)) {
        checkout_options_ctx *ctx = (checkout_options_ctx*) malloc(sizeof(checkout_options_ctx));
        ctx->env = env;
        ctx->callback = progress_callback;
        opts->progress_payload = ctx;
        opts->progress_cb = &checkout_progress_callback;
    }

    return em_nil;
}

static void checkout_options_release(git_checkout_options *opts)
{
    free(opts->notify_payload);
    free(opts->progress_payload);
}


// =============================================================================
// Checkout functions

#define PARSE_OPTIONS()                                 \
    do {                                                \
        checkout_options_parse(env, opts, &options);    \
        EM_RETURN_NIL_IF_NLE();                         \
    } while (0)

#define CLEANUP()                               \
    do {                                        \
        checkout_options_release(&options);     \
        EM_RETURN_NIL_IF_NLE();                 \
        if (retval == GIT_EUSER)                \
            return em_nil;                      \
        EGIT_CHECK_ERROR(retval);               \
        return em_nil;                          \
    } while (0)

EGIT_DOC(checkout_head, "REPO &optional OPTIONS",
         "Update files in the working tree of REPO to match the content of HEEAD.");
emacs_value egit_checkout_head(emacs_env *env, emacs_value _repo, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    git_checkout_options options;
    PARSE_OPTIONS();
    int retval = git_checkout_head(repo, &options);
    CLEANUP();
}

EGIT_DOC(checkout_index, "REPO &optional INDEX OPTIONS",
         "Update files in the working tree of REPO to match the content of INDEX.");
emacs_value egit_checkout_index(emacs_env *env, emacs_value _repo, emacs_value _index, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);

    if (EM_EXTRACT_BOOLEAN(_index))
        EGIT_ASSERT_INDEX(_index);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_index *index = EGIT_EXTRACT_OR_NULL(_index);

    git_checkout_options options;
    PARSE_OPTIONS();
    int retval = git_checkout_index(repo, index, &options);
    CLEANUP();
}

EGIT_DOC(checkout_tree, "REPO &optional TREEISH OPTIONS",
         "Update files in the index and working tree of REPO to match the content of TREEISH.");
emacs_value egit_checkout_tree(emacs_env *env, emacs_value _repo, emacs_value _treeish, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);

    // TODO: Do we need treeish assertions enough to make a macro?
    // Do we need to define a libgit-treeish-p?
    // If so, should it peel tags to confirm that the target actually is a tree or a commit?
    if (EM_EXTRACT_BOOLEAN(_treeish)) {
        egit_type treeish_type = egit_get_type(env, _treeish);
        if (treeish_type != EGIT_COMMIT && treeish_type != EGIT_TREE && treeish_type != EGIT_TAG) {
            em_signal_wrong_type(env, em_libgit_tree_p, _treeish);
            return em_nil;
        }
    }

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_object *treeish = EGIT_EXTRACT_OR_NULL(_treeish);

    git_checkout_options options;
    PARSE_OPTIONS();
    int retval = git_checkout_tree(repo, treeish, &options);
    CLEANUP();
}

#undef PARSE_OPTIONS
#undef CLEANUP
