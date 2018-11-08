#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-diff.h"


// =============================================================================
// Helpers - Diff Options

typedef struct {
    emacs_env *env;
    emacs_value notify_callback;
    emacs_value progress_callback;
} diff_options_ctx;

static int egit_diff_notify_callback(
    const git_diff *diff,
    const git_diff_delta *delta,
    const char *pathspec,
    void *payload)
{
    diff_options_ctx *ctx = (diff_options_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[3];
    args[0] = egit_wrap(env, EGIT_DIFF, diff, NULL);
    args[1] = egit_wrap(env, EGIT_DIFF_DELTA, delta, EM_EXTRACT_USER_PTR(args[0]));
    args[2] = env->make_string(env, pathspec, strlen(pathspec));
    emacs_value retval = env->funcall(env, ctx->notify_callback, 3, args);

    if (env->non_local_exit_check(env))
        return GIT_EUSER;
    if (env->eq(env, retval, em_abort))
        return GIT_EUSER;
    if (env->eq(env, retval, em_skip))
        return 1;
    return 0;
}

static int egit_diff_progress_callback(
    const git_diff *diff,
    const char *old_path,
    const char *new_path,
    void *payload)
{
    diff_options_ctx *ctx = (diff_options_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[3];
    args[0] = egit_wrap(env, EGIT_DIFF, diff, NULL);
    args[1] = env->make_string(env, old_path, strlen(old_path));
    args[2] = env->make_string(env, new_path, strlen(new_path));
    emacs_value retval = env->funcall(env, ctx->progress_callback, 3, args);

    if (env->non_local_exit_check(env))
        return GIT_EUSER;
    if (env->eq(env, retval, em_abort))
        return GIT_EUSER;
    return 0;
}

#define EQ(a, b) env->eq(env, a, b)
#define SET_BIT(tgt, bit, opt)                           \
    do {                                                 \
        if (EM_EXTRACT_BOOLEAN(opt))                     \
            (tgt) |= (bit);                              \
        else                                             \
            (tgt) &= ~(bit);                             \
    } while (0)

static emacs_value egit_diff_options_parse(emacs_env *env, emacs_value alist, git_diff_options *opts)
{
    int retval = git_diff_init_options(opts, GIT_DIFF_OPTIONS_VERSION);
    EGIT_CHECK_ERROR(retval);

    // Some options require additional parsing and/or allocation to fully integrate.
    // Since the main loop may exit, we save those here for later.
    emacs_value pathspec = em_nil;
    emacs_value notify_callback = em_nil, progress_callback = em_nil;
    emacs_value old_prefix = em_nil, new_prefix = em_nil;

    // Main loop through the options alist
    {
        emacs_value car, cdr;
        EM_DOLIST(option, alist, options);
        EM_ASSERT_CONS(option);

        car = em_car(env, option);
        cdr = em_cdr(env, option);

        if (EQ(car, em_reverse))
            SET_BIT(opts->flags, GIT_DIFF_REVERSE, cdr);
        else if (EQ(car, em_include_ignored))
            SET_BIT(opts->flags, GIT_DIFF_INCLUDE_IGNORED, cdr);
        else if (EQ(car, em_recurse_ignored_dirs))
            SET_BIT(opts->flags, GIT_DIFF_RECURSE_IGNORED_DIRS, cdr);
        else if (EQ(car, em_include_untracked))
            SET_BIT(opts->flags, GIT_DIFF_INCLUDE_UNTRACKED, cdr);
        else if (EQ(car, em_recurse_untracked_dirs))
            SET_BIT(opts->flags, GIT_DIFF_RECURSE_UNTRACKED_DIRS, cdr);
        else if (EQ(car, em_include_unmodified))
            SET_BIT(opts->flags, GIT_DIFF_INCLUDE_UNMODIFIED, cdr);
        else if (EQ(car, em_include_typechange))
            SET_BIT(opts->flags, GIT_DIFF_INCLUDE_TYPECHANGE, cdr);
        else if (EQ(car, em_include_typechange_trees))
            SET_BIT(opts->flags, GIT_DIFF_INCLUDE_TYPECHANGE_TREES, cdr);
        else if (EQ(car, em_ignore_filemode))
            SET_BIT(opts->flags, GIT_DIFF_IGNORE_FILEMODE, cdr);
        else if (EQ(car, em_ignore_submodules))
            SET_BIT(opts->flags, GIT_DIFF_IGNORE_SUBMODULES, cdr);
        else if (EQ(car, em_ignore_case))
            SET_BIT(opts->flags, GIT_DIFF_IGNORE_CASE, cdr);
        else if (EQ(car, em_include_casechange))
            SET_BIT(opts->flags, GIT_DIFF_INCLUDE_CASECHANGE, cdr);
        else if (EQ(car, em_disable_pathspec_match))
            SET_BIT(opts->flags, GIT_DIFF_DISABLE_PATHSPEC_MATCH, cdr);
        else if (EQ(car, em_skip_binary_check))
            SET_BIT(opts->flags, GIT_DIFF_SKIP_BINARY_CHECK, cdr);
        else if (EQ(car, em_enable_fast_untracked_dirs))
            SET_BIT(opts->flags, GIT_DIFF_ENABLE_FAST_UNTRACKED_DIRS, cdr);
        else if (EQ(car, em_update_index))
            SET_BIT(opts->flags, GIT_DIFF_UPDATE_INDEX, cdr);
        else if (EQ(car, em_include_unreadable))
            SET_BIT(opts->flags, GIT_DIFF_INCLUDE_UNREADABLE, cdr);
        else if (EQ(car, em_include_unreadable_as_untracked))
            SET_BIT(opts->flags, GIT_DIFF_INCLUDE_UNREADABLE_AS_UNTRACKED, cdr);
        else if (EQ(car, em_force_text))
            SET_BIT(opts->flags, GIT_DIFF_FORCE_TEXT, cdr);
        else if (EQ(car, em_force_binary))
            SET_BIT(opts->flags, GIT_DIFF_FORCE_BINARY, cdr);
        else if (EQ(car, em_ignore_whitespace))
            SET_BIT(opts->flags, GIT_DIFF_IGNORE_WHITESPACE, cdr);
        else if (EQ(car, em_ignore_whitespace_change))
            SET_BIT(opts->flags, GIT_DIFF_IGNORE_WHITESPACE_CHANGE, cdr);
        else if (EQ(car, em_ignore_whitespace_eol))
            SET_BIT(opts->flags, GIT_DIFF_IGNORE_WHITESPACE_EOL, cdr);
        else if (EQ(car, em_show_untracked_content))
            SET_BIT(opts->flags, GIT_DIFF_SHOW_UNTRACKED_CONTENT, cdr);
        else if (EQ(car, em_show_unmodified))
            SET_BIT(opts->flags, GIT_DIFF_SHOW_UNMODIFIED, cdr);
        else if (EQ(car, em_patience))
            SET_BIT(opts->flags, GIT_DIFF_PATIENCE, cdr);
        else if (EQ(car, em_minimal))
            SET_BIT(opts->flags, GIT_DIFF_MINIMAL, cdr);
        else if (EQ(car, em_show_binary))
            SET_BIT(opts->flags, GIT_DIFF_SHOW_BINARY, cdr);
        else if (EQ(car, em_indent_heuristic))
            SET_BIT(opts->flags, GIT_DIFF_INDENT_HEURISTIC, cdr);
        else if (EQ(car, em_ignore_submodules)) {
            if (!EM_EXTRACT_BOOLEAN(cdr))
                opts->ignore_submodules = GIT_SUBMODULE_IGNORE_UNSPECIFIED;
            else if (EQ(cdr, em_none))
                opts->ignore_submodules = GIT_SUBMODULE_IGNORE_NONE;
            else if (EQ(cdr, em_untracked))
                opts->ignore_submodules = GIT_SUBMODULE_IGNORE_UNTRACKED;
            else if (EQ(cdr, em_dirty))
                opts->ignore_submodules = GIT_SUBMODULE_IGNORE_DIRTY;
            else if (EQ(cdr, em_all))
                opts->ignore_submodules = GIT_SUBMODULE_IGNORE_ALL;
            else {
                em_signal_wrong_value(env, cdr);
                return em_nil;
            }
        }
        else if (EQ(car, em_pathspec))
            pathspec = cdr;  // Parse outside the main loop
        else if (EQ(car, em_notify)) {
            EM_ASSERT_FUNCTION(cdr);
            notify_callback = cdr;
        }
        else if (EQ(car, em_progress)) {
            EM_ASSERT_FUNCTION(cdr);
            progress_callback = cdr;
        }
        else if (EQ(car, em_context_lines)) {
            EM_ASSERT_INTEGER(cdr);
            opts->context_lines = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EQ(car, em_interhunk_lines)) {
            EM_ASSERT_INTEGER(cdr);
            opts->interhunk_lines = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EQ(car, em_id_abbrev)) {
            EM_ASSERT_INTEGER(cdr);
            opts->id_abbrev = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EQ(car, em_max_size)) {
            EM_ASSERT_INTEGER(cdr);
            opts->max_size = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EQ(car, em_old_prefix)) {
            EM_ASSERT_STRING(cdr);
            old_prefix = cdr;
        }
        else if (EQ(car, em_new_prefix)) {
            EM_ASSERT_STRING(cdr);
            new_prefix = cdr;
        }
        else {
            em_signal_wrong_value(env, car);
            return em_nil;
        }

        EM_DOLIST_END(options);
    }

    // Check the size of pathspec and ensure that it contains only strings
    size_t pathspec_size = 0;
    {
        EM_DOLIST(path, pathspec, paths_count);
        EM_ASSERT_STRING(path);
        pathspec_size++;
        EM_DOLIST_END(paths_count);
    }

    // At this point, we are certain that everything is in order, so we can start allocating
    if (pathspec_size > 0) {
        char **paths = (char**) malloc(pathspec_size * sizeof(char*));
        size_t path_index = 0;
        {
            EM_DOLIST(path, pathspec, paths_copy);
            paths[path_index++] = EM_EXTRACT_STRING(path);
            EM_DOLIST_END(paths_copy);
        }
        opts->pathspec.strings = paths;
        opts->pathspec.count = pathspec_size;
    }

    diff_options_ctx *callback_ctx = (diff_options_ctx*) malloc(sizeof(diff_options_ctx));
    callback_ctx->env = env;
    callback_ctx->notify_callback = notify_callback;
    callback_ctx->progress_callback = progress_callback;
    opts->payload = (void*) callback_ctx;

    if (EM_EXTRACT_BOOLEAN(notify_callback))
        opts->notify_cb = &egit_diff_notify_callback;
    if (EM_EXTRACT_BOOLEAN(notify_callback))
        opts->progress_cb = &egit_diff_progress_callback;

    if (EM_EXTRACT_BOOLEAN(old_prefix))
        opts->old_prefix = EM_EXTRACT_STRING(old_prefix);
    if (EM_EXTRACT_BOOLEAN(new_prefix))
        opts->new_prefix = EM_EXTRACT_STRING(new_prefix);

    return em_nil;
}

#undef EQ
#undef SET_BIT

static void egit_diff_options_release(git_diff_options *opts)
{
    free(opts->payload);
    for (size_t i = 0; i < opts->pathspec.count; i++)
        free(opts->pathspec.strings[i]);
    free(opts->pathspec.strings);
    free((char*) opts->new_prefix);
    free((char*) opts->old_prefix);
}


// =============================================================================
// Constructors

#define MAYBE_GET(type, to)                     \
    do {                                        \
        if (EM_EXTRACT_BOOLEAN(_##to)) {        \
            EGIT_ASSERT_##type(_##to);          \
            to = EGIT_EXTRACT(_##to);           \
        }                                       \
    } while(0)

#define PARSE_OPTIONS()                                 \
    do {                                                \
        egit_diff_options_parse(env, opts, &options);   \
        if (env->non_local_exit_check(env))             \
            return em_nil;                              \
    } while(0)

#define FINALIZE_AND_RETURN()                                \
    do {                                                     \
        egit_diff_options_release(&options);                 \
        if (env->non_local_exit_check(env))                  \
            return em_nil;                                   \
        if (retval == GIT_EUSER)                             \
            return em_nil;                                   \
        EGIT_CHECK_ERROR(retval);                            \
        return egit_wrap(env, EGIT_DIFF, diff, NULL);        \
    } while(0)

EGIT_DOC(diff_index_to_index, "REPO OLD-INDEX NEW-INDEX &optional OPTS",
         "Create a diff with the difference between two index objects.\n"
         "OLD-INDEX and NEW-INDEX must both belong to REPO.");
emacs_value egit_diff_index_to_index(
    emacs_env *env, emacs_value _repo, emacs_value _old_index,
    emacs_value _new_index, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_INDEX(_old_index);
    EGIT_ASSERT_INDEX(_new_index);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_index *old_index = EGIT_EXTRACT(_old_index);
    git_index *new_index = EGIT_EXTRACT(_new_index);

    git_diff_options options;
    PARSE_OPTIONS();

    git_diff *diff;
    int retval = git_diff_index_to_index(&diff, repo, old_index, new_index, &options);
    FINALIZE_AND_RETURN();
}

EGIT_DOC(diff_index_to_workdir, "REPO &optional INDEX OPTS",
         "Create a diff between an index and a workdir belonging to REPO.\n"
         "If INDEX wis nil, it will default to the repository index.");
emacs_value egit_diff_index_to_workdir(
    emacs_env *env, emacs_value _repo, emacs_value _index, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    git_index *index = NULL;
    MAYBE_GET(INDEX, index);

    git_diff_options options;
    PARSE_OPTIONS();

    git_diff *diff;
    int retval = git_diff_index_to_workdir(&diff, repo, index, &options);
    FINALIZE_AND_RETURN();
}

EGIT_DOC(diff_tree_to_index, "REPO &optional OLD-TREE INDEX OPTS",
         "Create a diff between a tree and an index belonging to REPO.\n"
         "If OLD-TREE or INDEX are nil, they will default to the empty tree\n"
         "or the repository index, respectively.");
emacs_value egit_diff_tree_to_index(
    emacs_env *env, emacs_value _repo, emacs_value _old_tree,
    emacs_value _index, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    git_tree *old_tree = NULL;
    MAYBE_GET(TREE, old_tree);

    git_index *index = NULL;
    MAYBE_GET(INDEX, index);

    git_diff_options options;
    PARSE_OPTIONS();

    git_diff *diff;
    int retval = git_diff_tree_to_index(&diff, repo, old_tree, index, &options);
    FINALIZE_AND_RETURN();
}

EGIT_DOC(diff_tree_to_tree, "REPO &optional OLD-TREE NEW-TREE OPTS",
         "Create a diff between two trees belonging to REPO.\n"
         "If OLD-TREE or NEW-TREE are nil, they default to the empty tree.");
emacs_value egit_diff_tree_to_tree(
    emacs_env *env, emacs_value _repo, emacs_value _old_tree,
    emacs_value _new_tree, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    git_tree *old_tree = NULL, *new_tree = NULL;
    MAYBE_GET(TREE, old_tree);
    MAYBE_GET(TREE, new_tree);

    git_diff_options options;
    PARSE_OPTIONS();

    git_diff *diff;
    int retval = git_diff_tree_to_tree(&diff, repo, old_tree, new_tree, &options);
    FINALIZE_AND_RETURN();
}

EGIT_DOC(diff_tree_to_workdir, "REPO &optional OLD-TREE OPTS",
         "Create a diff between OLD-TREE and the working directory of REPO.\n"
         "If OLD-TREE is nil it will default to the empty tree.");
emacs_value egit_diff_tree_to_workdir(
    emacs_env *env, emacs_value _repo, emacs_value _old_tree, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    git_tree *old_tree = NULL;
    MAYBE_GET(TREE, old_tree);

    git_diff_options options;
    PARSE_OPTIONS();

    git_diff *diff;
    int retval = git_diff_tree_to_workdir(&diff, repo, old_tree, &options);
    FINALIZE_AND_RETURN();
}

EGIT_DOC(diff_tree_to_workdir_with_index, "REPO &optional OLD-TREE OPTS",
         "Create a diff between OLD-TREE and the working directory of REPO\n"
         "using index data to account for staged deletes, tracked files, etc.\n"
         "If OLD-TREE is nil it will default to the empty tree.");
emacs_value egit_diff_tree_to_workdir_with_index(
    emacs_env *env, emacs_value _repo, emacs_value _old_tree, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    git_tree *old_tree = NULL;
    MAYBE_GET(TREE, old_tree);

    git_diff_options options;
    PARSE_OPTIONS();

    git_diff *diff;
    int retval = git_diff_tree_to_workdir_with_index(&diff, repo, old_tree, &options);
    FINALIZE_AND_RETURN();
}

#undef MAYBE_GET
#undef PARSE_OPTIONS
#undef FINALIZE_AND_RETURN
