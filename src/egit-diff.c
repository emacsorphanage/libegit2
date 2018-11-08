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


// =============================================================================
// Foreach

typedef struct {
    emacs_env *env;
    egit_object *diff_wrapper;
    emacs_value file_callback;
    emacs_value binary_callback;
    emacs_value hunk_callback;
    emacs_value line_callback;
} diff_foreach_ctx;

static int egit_diff_foreach_file_callback(const git_diff_delta *delta, float progress, void *payload)
{
    diff_foreach_ctx *ctx = (diff_foreach_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[2];
    args[0] = egit_wrap(env, EGIT_DIFF_DELTA, delta, ctx->diff_wrapper);
    args[1] = env->make_float(env, progress);
    emacs_value retval = env->funcall(env, ctx->file_callback, 2, args);

    if (env->non_local_exit_check(env))
        return GIT_EUSER;
    if (env->eq(env, retval, em_abort))
        return GIT_EUSER;
    return 0;
}

static int egit_diff_foreach_binary_callback(
    const git_diff_delta *delta, const git_diff_binary *binary, void *payload)
{
    diff_foreach_ctx *ctx = (diff_foreach_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[2];
    args[0] = egit_wrap(env, EGIT_DIFF_DELTA, delta, ctx->diff_wrapper);
    args[1] = egit_wrap(env, EGIT_DIFF_BINARY, binary, ctx->diff_wrapper);
    emacs_value retval = env->funcall(env, ctx->binary_callback, 2, args);

    if (env->non_local_exit_check(env))
        return GIT_EUSER;
    if (env->eq(env, retval, em_abort))
        return GIT_EUSER;
    return 0;
}

static int egit_diff_foreach_hunk_callback(
    const git_diff_delta *delta, const git_diff_hunk *hunk, void *payload)
{
    diff_foreach_ctx *ctx = (diff_foreach_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[2];
    args[0] = egit_wrap(env, EGIT_DIFF_DELTA, delta, ctx->diff_wrapper);
    args[1] = egit_wrap(env, EGIT_DIFF_HUNK, hunk, ctx->diff_wrapper);
    emacs_value retval = env->funcall(env, ctx->hunk_callback, 2, args);

    if (env->non_local_exit_check(env))
        return GIT_EUSER;
    if (env->eq(env, retval, em_abort))
        return GIT_EUSER;
    return 0;
}

static int egit_diff_foreach_line_callback(
    const git_diff_delta *delta, const git_diff_hunk *hunk,
    const git_diff_line *line, void *payload)
{
    diff_foreach_ctx *ctx = (diff_foreach_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[3];
    args[0] = egit_wrap(env, EGIT_DIFF_DELTA, delta, ctx->diff_wrapper);
    args[1] = egit_wrap(env, EGIT_DIFF_HUNK, hunk, ctx->diff_wrapper);
    args[2] = egit_wrap(env, EGIT_DIFF_LINE, line, ctx->diff_wrapper);
    emacs_value retval = env->funcall(env, ctx->line_callback, 3, args);

    if (env->non_local_exit_check(env))
        return GIT_EUSER;
    if (env->eq(env, retval, em_abort))
        return GIT_EUSER;
    return 0;
}

EGIT_DOC(diff_foreach, "DIFF FILE-FUNC &optional BINARY-FUNC HUNK-FUNC LINE-FUNC",
         "Loop over all deltas in a diff while issuing callbacks.\n"
         "- FILE-FUNC will be called for each file in the diff.\n"
         "- BINARY-FUNC will be called for binary files.\n"
         "- HUNK-FUNC will be called for ranges of lines in a diff.\n"
         "- LINE-FUNC will be called per line of diff text.");
emacs_value egit_diff_foreach(
    emacs_env *env, emacs_value _diff, emacs_value file_cb,
    emacs_value binary_cb, emacs_value hunk_cb, emacs_value line_cb)
{
    EGIT_ASSERT_DIFF(_diff);
    EM_ASSERT_FUNCTION(file_cb);
    if (EM_EXTRACT_BOOLEAN(binary_cb)) EM_ASSERT_FUNCTION(binary_cb);
    if (EM_EXTRACT_BOOLEAN(hunk_cb)) EM_ASSERT_FUNCTION(hunk_cb);
    if (EM_EXTRACT_BOOLEAN(line_cb)) EM_ASSERT_FUNCTION(line_cb);

    git_diff *diff = EGIT_EXTRACT(_diff);
    diff_foreach_ctx *ctx = (diff_foreach_ctx*) malloc(sizeof(diff_foreach_ctx));
    *ctx = (diff_foreach_ctx) {env, EM_EXTRACT_USER_PTR(_diff), file_cb, binary_cb, hunk_cb, line_cb};

    int retval = git_diff_foreach(
        diff,
        &egit_diff_foreach_file_callback,
        EM_EXTRACT_BOOLEAN(binary_cb) ? &egit_diff_foreach_binary_callback : NULL,
        EM_EXTRACT_BOOLEAN(hunk_cb) ? &egit_diff_foreach_hunk_callback : NULL,
        EM_EXTRACT_BOOLEAN(line_cb) ? &egit_diff_foreach_line_callback : NULL,
        ctx
    );
    free(ctx);

    if (env->non_local_exit_check(env))
        return em_nil;
    if (retval == GIT_EUSER)
        return em_nil;
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}


// =============================================================================
// Getters - delta

EGIT_DOC(diff_delta_file_id, "DELTA SIDE",
         "Get the file ID of a side in DELTA.\n"
         "SIDE must be either `old' or `new'.");
emacs_value egit_diff_delta_file_id(emacs_env *env, emacs_value _delta, emacs_value side)
{
    EGIT_ASSERT_DIFF_DELTA(_delta);
    git_diff_delta *delta = EGIT_EXTRACT(_delta);
    const git_oid *oid = env->eq(env, side, em_old) ? &delta->old_file.id : &delta->new_file.id;
    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}

EGIT_DOC(diff_delta_file_path, "DELTA SIDE",
         "Get the file path of a side in DELTA.\n"
         "SIDE must be either `old' or `new'.");
emacs_value egit_diff_delta_file_path(emacs_env *env, emacs_value _delta, emacs_value side)
{
    EGIT_ASSERT_DIFF_DELTA(_delta);
    git_diff_delta *delta = EGIT_EXTRACT(_delta);
    const char *path = env->eq(env, side, em_old) ? delta->old_file.path : delta->new_file.path;
    return env->make_string(env, path, strlen(path));
}

EGIT_DOC(diff_delta_nfiles, "DELTA", "Get the number of files in DELTA.");
emacs_value egit_diff_delta_nfiles(emacs_env *env, emacs_value _delta)
{
    EGIT_ASSERT_DIFF_DELTA(_delta);
    git_diff_delta *delta = EGIT_EXTRACT(_delta);
    return env->make_integer(env, delta->nfiles);
}

EGIT_DOC(diff_delta_similarity, "DELTA",
         "Get similarity score of DELTA.\n"
         "This only makes sense for status `renamed' and `copied', and will\n"
         "be unavailable unless a similarity check has been run on the diff.");
emacs_value egit_diff_delta_similarity(emacs_env *env, emacs_value _delta)
{
    EGIT_ASSERT_DIFF_DELTA(_delta);
    git_diff_delta *delta = EGIT_EXTRACT(_delta);
    return env->make_integer(env, delta->similarity);
}

EGIT_DOC(diff_delta_status, "DELTA",
         "Get the status of DELTA.\n"
         "The available statuses are `unmodified', `added', `deleted', `modified',\n"
         "`renamed', `copied', `ignored', `untracked', `typechange', `unreadable',\n"
         "and `conflicted'.");
emacs_value egit_diff_delta_status(emacs_env *env, emacs_value _delta)
{
    EGIT_ASSERT_DIFF_DELTA(_delta);
    git_diff_delta *delta = EGIT_EXTRACT(_delta);

    switch (delta->status) {
    case GIT_DELTA_UNMODIFIED: return em_unmodified;
    case GIT_DELTA_ADDED: return em_added;
    case GIT_DELTA_DELETED: return em_deleted;
    case GIT_DELTA_MODIFIED: return em_modified;
    case GIT_DELTA_RENAMED: return em_renamed;
    case GIT_DELTA_COPIED: return em_copied;
    case GIT_DELTA_IGNORED: return em_ignored;
    case GIT_DELTA_UNTRACKED: return em_untracked;
    case GIT_DELTA_TYPECHANGE: return em_typechange;
    case GIT_DELTA_UNREADABLE: return em_unreadable;
    case GIT_DELTA_CONFLICTED: return em_conflicted;
    }

    return em_nil;  // Should be unreachable
}


// =============================================================================
// Predicates - delta

EGIT_DOC(diff_delta_file_exists_p, "DELTA SIDE",
         "Non-nil if the file exists on this side of DELTA.\n"
         "SIDE must be either `old' or `new'.");
emacs_value egit_diff_delta_file_exists_p(emacs_env *env, emacs_value _delta, emacs_value side)
{
    EGIT_ASSERT_DIFF_DELTA(_delta);
    git_diff_delta *delta = EGIT_EXTRACT(_delta);
    int flags = env->eq(env, side, em_old) ? delta->old_file.flags : delta->new_file.flags;
    return (flags & GIT_DIFF_FLAG_EXISTS) ? em_t : em_nil;
}


// =============================================================================
// Getters - hunk

EGIT_DOC(diff_hunk_header, "HUNK", "Return the header of HUNK.");
emacs_value egit_diff_hunk_header(emacs_env *env, emacs_value _hunk)
{
    EGIT_ASSERT_DIFF_HUNK(_hunk);
    git_diff_hunk *hunk = EGIT_EXTRACT(_hunk);
    return env->make_string(env, &hunk->header[0], hunk->header_len);
}

EGIT_DOC(diff_hunk_lines, "HUNK SIDE",
         "Return the number of lines of HUNK.\n"
         "SIDE must be either `old' or `new'.");
emacs_value egit_diff_hunk_lines(emacs_env *env, emacs_value _hunk, emacs_value side)
{
    EGIT_ASSERT_DIFF_HUNK(_hunk);
    git_diff_hunk *hunk = EGIT_EXTRACT(_hunk);
    int num = env->eq(env, side, em_old) ? hunk->old_lines : hunk->new_lines;
    return env->make_integer(env, num);
}

EGIT_DOC(diff_hunk_start, "HUNK SIDE",
         "Return starting line number of HUNK.\n"
         "SIDE must be either `old' or `new'.");
emacs_value egit_diff_hunk_start(emacs_env *env, emacs_value _hunk, emacs_value side)
{
    EGIT_ASSERT_DIFF_HUNK(_hunk);
    git_diff_hunk *hunk = EGIT_EXTRACT(_hunk);
    int num = env->eq(env, side, em_old) ? hunk->old_start : hunk->new_start;
    return env->make_integer(env, num);
}


// =============================================================================
// Getters - line

EGIT_DOC(diff_line_origin, "LINE",
         "Get the origin of LINE.\n"
         "This is a single character (an integer) among the following:\n"
         "- ' ' for context lines\n"
         "- '+' for added lines\n"
         "- '-' for removed lines\n"
         "- '=' (context) both files have no LF at end\n"
         "- '>' old file has no LF at end\n"
         "- '<' new file has no LF at end\n"
         "- 'F' file header\n"
         "- 'H' hunk header\n"
         "- 'B' binary files differ\n\n"
         "The last three values are only sent to line callbacks when the content\n"
         "of a diff is being filtered through `libgit-diff-print'.");
emacs_value egit_diff_line_origin(emacs_env *env, emacs_value _line)
{
    EGIT_ASSERT_DIFF_LINE(_line);
    git_diff_line *line = EGIT_EXTRACT(_line);
    return env->make_integer(env, line->origin);
}

EGIT_DOC(diff_line_lineno, "LINE SIDE",
         "Line number of LINE on one side of the diff.\n"
         "SIDE must be either `old' or `new'.\n"
         "If LINE was added or removed, the return value is nil\n"
         "if SIDE is `old' or `new', respectively.");
emacs_value egit_diff_line_lineno(emacs_env *env, emacs_value _line, emacs_value side)
{
    EGIT_ASSERT_DIFF_LINE(_line);
    git_diff_line *line = EGIT_EXTRACT(_line);
    int lineno = env->eq(env, side, em_old) ? line->old_lineno : line->new_lineno;
    return env->make_integer(env, lineno);
}

EGIT_DOC(diff_line_content, "LINE", "Get the content of LINE as a string.");
emacs_value egit_diff_line_content(emacs_env *env, emacs_value _line)
{
    EGIT_ASSERT_DIFF_LINE(_line);
    git_diff_line *line = EGIT_EXTRACT(_line);
    return env->make_string(env, line->content, line->content_len);
}


// =============================================================================
// Other getters

EGIT_DOC(diff_get_delta, "DIFF N", "Get the Nth delta from DIFF.");
emacs_value egit_diff_get_delta(emacs_env *env, emacs_value _diff, emacs_value _index)
{
    EGIT_ASSERT_DIFF(_diff);
    EM_ASSERT_INTEGER(_index);
    git_diff *diff = EGIT_EXTRACT(_diff);
    intmax_t index = EM_EXTRACT_INTEGER(_index);
    const git_diff_delta *delta = git_diff_get_delta(diff, index);
    if (!delta) {
        em_signal_args_out_of_range(env, index);
        return em_nil;
    }
    return egit_wrap(env, EGIT_DIFF_DELTA, delta, EM_EXTRACT_USER_PTR(_diff));
}

EGIT_DOC(diff_num_deltas, "DIFF &optional TYPE",
         "Get the number of deltas in DIFF.\n"
         "If TYPE is given, get only the number of deltas with that type.\n"
         "The available types are `unmodified', `added', `deleted', `modified',\n"
         "`renamed', `copied', `ignored', `untracked', `typechange', `unreadable',\n"
         "and `conflicted'.");
emacs_value egit_diff_num_deltas(emacs_env *env, emacs_value _diff, emacs_value _type)
{
    EGIT_ASSERT_DIFF(_diff);
    git_diff *diff = EGIT_EXTRACT(_diff);

    size_t num;
    if (!EM_EXTRACT_BOOLEAN(_type))
        num = git_diff_num_deltas(diff);
    else if (env->eq(env, _type, em_unmodified))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_UNMODIFIED);
    else if (env->eq(env, _type, em_added))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_ADDED);
    else if (env->eq(env, _type, em_deleted))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_DELETED);
    else if (env->eq(env, _type, em_modified))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_MODIFIED);
    else if (env->eq(env, _type, em_renamed))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_RENAMED);
    else if (env->eq(env, _type, em_copied))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_COPIED);
    else if (env->eq(env, _type, em_ignored))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_IGNORED);
    else if (env->eq(env, _type, em_untracked))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_UNTRACKED);
    else if (env->eq(env, _type, em_typechange))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_TYPECHANGE);
    else if (env->eq(env, _type, em_unreadable))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_UNREADABLE);
    else if (env->eq(env, _type, em_conflicted))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_CONFLICTED);
    else {
        em_signal_wrong_value(env, _type);
        return em_nil;
    }

    return env->make_integer(env, num);
}
