#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
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
    args[2] = EM_STRING(pathspec);
    emacs_value retval = env->funcall(env, ctx->notify_callback, 3, args);

    EM_RETURN_IF_NLE(GIT_EUSER);
    if (EM_EQ(retval, em_abort))
        return GIT_EUSER;
    if (EM_EQ(retval, em_skip))
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
    args[1] = EM_STRING(old_path);
    args[2] = EM_STRING(new_path);
    emacs_value retval = env->funcall(env, ctx->progress_callback, 3, args);

    EM_RETURN_IF_NLE(GIT_EUSER);
    if (EM_EQ(retval, em_abort))
        return GIT_EUSER;
    return 0;
}

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

        if (EM_EQ(car, em_reverse))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_REVERSE, cdr);
        else if (EM_EQ(car, em_include_ignored))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_INCLUDE_IGNORED, cdr);
        else if (EM_EQ(car, em_recurse_ignored_dirs))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_RECURSE_IGNORED_DIRS, cdr);
        else if (EM_EQ(car, em_include_untracked))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_INCLUDE_UNTRACKED, cdr);
        else if (EM_EQ(car, em_recurse_untracked_dirs))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_RECURSE_UNTRACKED_DIRS, cdr);
        else if (EM_EQ(car, em_include_unmodified))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_INCLUDE_UNMODIFIED, cdr);
        else if (EM_EQ(car, em_include_typechange))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_INCLUDE_TYPECHANGE, cdr);
        else if (EM_EQ(car, em_include_typechange_trees))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_INCLUDE_TYPECHANGE_TREES, cdr);
        else if (EM_EQ(car, em_ignore_filemode))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_IGNORE_FILEMODE, cdr);
        else if (EM_EQ(car, em_ignore_submodules))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_IGNORE_SUBMODULES, cdr);
        else if (EM_EQ(car, em_ignore_case))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_IGNORE_CASE, cdr);
        else if (EM_EQ(car, em_include_casechange))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_INCLUDE_CASECHANGE, cdr);
        else if (EM_EQ(car, em_disable_pathspec_match))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_DISABLE_PATHSPEC_MATCH, cdr);
        else if (EM_EQ(car, em_skip_binary_check))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_SKIP_BINARY_CHECK, cdr);
        else if (EM_EQ(car, em_enable_fast_untracked_dirs))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_ENABLE_FAST_UNTRACKED_DIRS, cdr);
        else if (EM_EQ(car, em_update_index))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_UPDATE_INDEX, cdr);
        else if (EM_EQ(car, em_include_unreadable))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_INCLUDE_UNREADABLE, cdr);
        else if (EM_EQ(car, em_include_unreadable_as_untracked))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_INCLUDE_UNREADABLE_AS_UNTRACKED, cdr);
        else if (EM_EQ(car, em_force_text))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_FORCE_TEXT, cdr);
        else if (EM_EQ(car, em_force_binary))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_FORCE_BINARY, cdr);
        else if (EM_EQ(car, em_ignore_whitespace))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_IGNORE_WHITESPACE, cdr);
        else if (EM_EQ(car, em_ignore_whitespace_change))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_IGNORE_WHITESPACE_CHANGE, cdr);
        else if (EM_EQ(car, em_ignore_whitespace_eol))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_IGNORE_WHITESPACE_EOL, cdr);
        else if (EM_EQ(car, em_show_untracked_content))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_SHOW_UNTRACKED_CONTENT, cdr);
        else if (EM_EQ(car, em_show_unmodified))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_SHOW_UNMODIFIED, cdr);
        else if (EM_EQ(car, em_patience))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_PATIENCE, cdr);
        else if (EM_EQ(car, em_minimal))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_MINIMAL, cdr);
        else if (EM_EQ(car, em_show_binary))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_SHOW_BINARY, cdr);
        else if (EM_EQ(car, em_indent_heuristic))
            EGIT_SET_BIT(opts->flags, GIT_DIFF_INDENT_HEURISTIC, cdr);
        else if (EM_EQ(car, em_ignore_submodules)) {
            if (!EM_EXTRACT_BOOLEAN(cdr))
                opts->ignore_submodules = GIT_SUBMODULE_IGNORE_UNSPECIFIED;
            else if (EM_EQ(cdr, em_none))
                opts->ignore_submodules = GIT_SUBMODULE_IGNORE_NONE;
            else if (EM_EQ(cdr, em_untracked))
                opts->ignore_submodules = GIT_SUBMODULE_IGNORE_UNTRACKED;
            else if (EM_EQ(cdr, em_dirty))
                opts->ignore_submodules = GIT_SUBMODULE_IGNORE_DIRTY;
            else if (EM_EQ(cdr, em_all))
                opts->ignore_submodules = GIT_SUBMODULE_IGNORE_ALL;
            else {
                em_signal_wrong_value(env, cdr);
                return em_nil;
            }
        }
        else if (EM_EQ(car, em_pathspec))
            pathspec = cdr;  // Parse outside the main loop
        else if (EM_EQ(car, em_notify)) {
            EM_ASSERT_FUNCTION(cdr);
            notify_callback = cdr;
        }
        else if (EM_EQ(car, em_progress)) {
            EM_ASSERT_FUNCTION(cdr);
            progress_callback = cdr;
        }
        else if (EM_EQ(car, em_context_lines)) {
            EM_ASSERT_INTEGER(cdr);
            opts->context_lines = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EM_EQ(car, em_interhunk_lines)) {
            EM_ASSERT_INTEGER(cdr);
            opts->interhunk_lines = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EM_EQ(car, em_id_abbrev)) {
            EM_ASSERT_INTEGER(cdr);
            opts->id_abbrev = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EM_EQ(car, em_max_size)) {
            EM_ASSERT_INTEGER(cdr);
            opts->max_size = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EM_EQ(car, em_old_prefix)) {
            EM_ASSERT_STRING(cdr);
            old_prefix = cdr;
        }
        else if (EM_EQ(car, em_new_prefix)) {
            EM_ASSERT_STRING(cdr);
            new_prefix = cdr;
        }
        else {
            em_signal_wrong_value(env, car);
            return em_nil;
        }

        EM_DOLIST_END(options);
    }

    if (!egit_strarray_from_list(&opts->pathspec, env, pathspec))
        return em_nil;

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

static void egit_diff_options_release(git_diff_options *opts)
{
    egit_strarray_dispose(&opts->pathspec);
    free(opts->payload);
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
        EM_RETURN_NIL_IF_NLE();                         \
    } while(0)

#define FINALIZE_AND_RETURN()                                \
    do {                                                     \
        egit_diff_options_release(&options);                 \
        EM_RETURN_NIL_IF_NLE();                              \
        if (retval == GIT_EUSER)                             \
            return em_nil;                                   \
        EGIT_CHECK_ERROR(retval);                            \
        return egit_wrap(env, EGIT_DIFF, diff, NULL);        \
    } while(0)

EGIT_DOC(diff_index_to_index, "REPO OLD-INDEX NEW-INDEX &optional OPTS",
         "Create a diff with the difference between two index objects.\n"
         "OLD-INDEX and NEW-INDEX must both belong to REPO.\n\n"
         "OPTS is an alist of options, with the following allowed keys:\n"
         "- `reverse': if non-nil, swap new and old\n"
         "- `include-ignored': if non-nil, include ignored files\n"
         "- `recurse-ignored-dirs': if non-nil, recurse through ignored dirs\n"
         "- `include-untracked': f non-nil, include untracked files in the diff\n"
         "- `recurse-untracked-dirs': if non-nil, recurse through untracked dirs\n"
         "- `include-unmodified': if non-nil, include unmodified files\n"
         "- `include-typechange': if non-nil, enable the generation of\n"
         "      `typechange' deltas, instead of an `added' and a `deleted'\n"
         "- `include-typechange-trees': if non-nil, try to correctly label\n"
         "      blob -> tree transitions as `typechange' records\n"
         "- `ignore-filemode': if non-nil, ignore filemode changes\n"
         "- `ignore-submodules': if non-nil, treat all submodules as unmodified\n"
         "- `ignore-case': if non-nil, use case insensitive filenames\n"
         "- `include-casechange': if non-nil, combined with `ignore-case',\n"
         "      specifies that a file with changed case is returned as an\n"
         "      add/delete pair.\n"
         "- `disable-pathspec-match': if non-nil, and if the options include\n"
         "      a pathspec, this indicates that the paths are treated as literal\n"
         "      instead of as match patterns.\n"
         "- `skip-binary-check': if non-nil, disable updating of the binary flag\n"
         "- `enable-fast-untracked-dirs': if non-nil, disables the extra check in\n"
         "      untracked directories for untracked or ignored files\n"
         "- `diff-update-index': if non-nil, update the index if a file is\n"
         "      found with the same OID as in the index, but with different\n"
         "      stat information.\n"
         "- `include-unreadable': if non-nil, include unreadable files\n"
         "- `include-unreadable-as-untracked': if non-nil, include unreadable\n"
         "      files as untracked ones.\n"
         "- `indent-heuristic': if non-nil, use a heuristic that takes indentation\n"
         "      and whitespace into account.\n"
         "- `force-text': if non-nil, treat all files as text\n"
         "- `force-binary': if non-nil, treat all files as binary\n"
         "- `ignore-whitespace': if non-nil, ignore all whitespace\n"
         "- `ignore-whitespace-change': if non-nil, ignore changes in amount of\n"
         "      whitespace\n"
         "- `ignore-whitespace-eol': if non-nil, ignore whitespace at end of line\n"
         "- `show-untracked-content': if non-nil, include content of untracked files;\n"
         "      this enables `include-untracked' but not `include-untracked-dirs'"
         "- `show-unmodified': if non-nil, names of unmodified files\n"
         "- `patience': if non-nil, use the 'patience diff' algorithm\n"
         "- `minimal': if non-nil, take extra time to find a minimal diff\n"
         "- `show-binary': if non-nil, include deflate/delta information\n"
         "- `ignore-submodules': can take any of the values `nil', `none',\n"
         "      `untracked', `dirty', and `all'.\n"
         "- `pathspec': a list of path patterns or literal paths to constrain the diff\n"
         "- `context-lines': number of context lines to generate (default 3)\n"
         "- `interhunk-lines': maximum number of unchanged lines between hunks before\n"
         "      hunks are merged into one (default 0)\n"
         "- `id-abbrev': abbreviation length when formatting object IDs (default 7)\n"
         "- `max-size': size (in bytes) above which a blob is automatically marked as\n"
         "      binary (default 512MB)\n"
         "- `old-prefix': virtual directory for old file names (default 'a')\n"
         "- `new-prefix': virtual directory for new file names (default 'b')\n"
         "- `notify': callback function for notification of new deltas;\n"
         "      will be called with three arguments: the diff so far, the new delta\n"
         "      and the matched path\n"
         "- `progress': callback function for progress tracking;\n"
         "      will be called with three arguments: the diff so far, the path to the\n"
         "      old file being checked, and the path to the new file.");
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
         "If INDEX wis nil, it will default to the repository index.\n"
         "See `libgit-diff-index-to-index' for explanation of OPTS.");
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
         "or the repository index, respectively.\n"
         "See `libgit-diff-index-to-index' for explanation of OPTS.");
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
         "If OLD-TREE or NEW-TREE are nil, they default to the empty tree.\n"
         "See `libgit-diff-index-to-index' for explanation of OPTS.");
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
         "If OLD-TREE is nil it will default to the empty tree.\n"
         "See `libgit-diff-index-to-index' for explanation of OPTS.");
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
         "If OLD-TREE is nil it will default to the empty tree.\n"
         "See `libgit-diff-index-to-index' for explanation of OPTS.");
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

    EM_RETURN_IF_NLE(GIT_EUSER);
    if (EM_EQ(retval, em_abort))
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

    EM_RETURN_IF_NLE(GIT_EUSER);
    if (EM_EQ(retval, em_abort))
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

    EM_RETURN_IF_NLE(GIT_EUSER);
    if (EM_EQ(retval, em_abort))
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

    EM_RETURN_IF_NLE(GIT_EUSER);
    if (EM_EQ(retval, em_abort))
        return GIT_EUSER;
    return 0;
}

EGIT_DOC(diff_foreach, "DIFF FILE-FUNC &optional BINARY-FUNC HUNK-FUNC LINE-FUNC",
         "Loop over all deltas in a diff while issuing callbacks.\n"
         "- FILE-FUNC will be called for each file in the diff.\n"
         "- BINARY-FUNC will be called for binary files.\n"
         "- HUNK-FUNC will be called for ranges of lines in a diff.\n"
         "- LINE-FUNC will be called per line of diff text.\n\n"
         "FILE-FUNC receives two arguments: a delta object and a progress\n"
         "floating point number that goes from 0 to 1 over the diff.\n\n"
         "BINARY-FUNC and HUNK-FUNC receives a delta object and a binary\n"
         "or a hunk object, respectively.\n\n"
         "LINE-FUNC receives a delta object, a hunk object and a line object.\n\n"
         "NOTE: Binary, hunk and line objects have lifetimes that are limited to\n"
         "a single function call!");
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
    diff_foreach_ctx ctx = {env, EM_EXTRACT_USER_PTR(_diff), file_cb, binary_cb, hunk_cb, line_cb};

    int retval = git_diff_foreach(
        diff,
        &egit_diff_foreach_file_callback,
        EM_EXTRACT_BOOLEAN(binary_cb) ? &egit_diff_foreach_binary_callback : NULL,
        EM_EXTRACT_BOOLEAN(hunk_cb) ? &egit_diff_foreach_hunk_callback : NULL,
        EM_EXTRACT_BOOLEAN(line_cb) ? &egit_diff_foreach_line_callback : NULL,
        &ctx
    );

    EM_RETURN_NIL_IF_NLE();
    if (retval == GIT_EUSER)
        return em_nil;
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}


// =============================================================================
// Print

typedef struct {
    emacs_env *env;
    egit_object *diff_wrapper;
    emacs_value line_callback;
} diff_print_ctx;

int egit_diff_print_line_callback(
    const git_diff_delta *delta, const git_diff_hunk *hunk,
    const git_diff_line *line, void *payload)
{
    diff_print_ctx *ctx = (diff_print_ctx*) payload;
    emacs_env *env = ctx->env;

    if (!EM_EXTRACT_BOOLEAN(ctx->line_callback)) {
        // Default choice: write to buffer
        if (line->origin == ' ' || line->origin == '+' || line->origin == '-')
            em_insert(env, &line->origin, 1);
        em_insert(env, line->content, line->content_len);

        EM_RETURN_IF_NLE(GIT_EUSER);
        return 0;
    }

    emacs_value args[3];
    args[0] = egit_wrap(env, EGIT_DIFF_DELTA, delta, ctx->diff_wrapper);
    args[1] = egit_wrap(env, EGIT_DIFF_HUNK, hunk, ctx->diff_wrapper);
    args[2] = egit_wrap(env, EGIT_DIFF_LINE, line, ctx->diff_wrapper);
    emacs_value retval = env->funcall(env, ctx->line_callback, 3, args);

    EM_RETURN_IF_NLE(GIT_EUSER);
    if (EM_EQ(retval, em_abort))
        return GIT_EUSER;
    return 0;
}

EGIT_DOC(diff_print, "DIFF &optional FORMAT LINE-FUNC",
         "Iterate through DIFF calling LINE-FUNC on each line.\n"
         "FORMAT is one of the symbols `patch' (default), `patch-header',\n"
         "`raw', `name-only' and `name-status'.\n"
         "LINE-FUNC is called with three arguments: a delta, hunk and a line\n"
         "object. The default will issue a call to `insert' that is suitable\n"
         "for printing a diff to the current buffer.\n\n"
         "NOTE: Hunk and line objects have lifetimes that are limited to\n"
         "a single function call!");
emacs_value egit_diff_print(
    emacs_env *env, emacs_value _diff, emacs_value _format, emacs_value func)
{
    EGIT_ASSERT_DIFF(_diff);
    if (EM_EXTRACT_BOOLEAN(func)) EM_ASSERT_FUNCTION(func);

    git_diff_format_t format;
    if (!EM_EXTRACT_BOOLEAN(_format) || EM_EQ(_format, em_patch))
        format = GIT_DIFF_FORMAT_PATCH;
    else if (EM_EQ(_format, em_patch_header))
        format = GIT_DIFF_FORMAT_PATCH_HEADER;
    else if (EM_EQ(_format, em_raw))
        format = GIT_DIFF_FORMAT_RAW;
    else if (EM_EQ(_format, em_name_only))
        format = GIT_DIFF_FORMAT_NAME_ONLY;
    else if (EM_EQ(_format, em_name_status))
        format = GIT_DIFF_FORMAT_NAME_STATUS;
    else {
        em_signal_wrong_value(env, _format);
        return em_nil;
    }

    git_diff *diff = EGIT_EXTRACT(_diff);
    diff_print_ctx ctx = {env, EM_EXTRACT_USER_PTR(_diff), func};

    int retval = git_diff_print(diff, format, &egit_diff_print_line_callback, &ctx);

    EM_RETURN_NIL_IF_NLE();
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
    const git_oid *oid = EM_EQ(side, em_old) ? &delta->old_file.id : &delta->new_file.id;
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(diff_delta_file_path, "DELTA SIDE",
         "Get the file path of a side in DELTA.\n"
         "SIDE must be either `old' or `new'.");
emacs_value egit_diff_delta_file_path(emacs_env *env, emacs_value _delta, emacs_value side)
{
    EGIT_ASSERT_DIFF_DELTA(_delta);
    git_diff_delta *delta = EGIT_EXTRACT(_delta);
    const char *path = EM_EQ(side, em_old) ? delta->old_file.path : delta->new_file.path;
    return EM_STRING(path);
}

EGIT_DOC(diff_delta_nfiles, "DELTA", "Get the number of files in DELTA.");
emacs_value egit_diff_delta_nfiles(emacs_env *env, emacs_value _delta)
{
    EGIT_ASSERT_DIFF_DELTA(_delta);
    git_diff_delta *delta = EGIT_EXTRACT(_delta);
    return EM_INTEGER(delta->nfiles);
}

EGIT_DOC(diff_delta_similarity, "DELTA",
         "Get similarity score of DELTA.\n"
         "This only makes sense for status `renamed' and `copied', and will\n"
         "be unavailable unless a similarity check has been run on the diff.");
emacs_value egit_diff_delta_similarity(emacs_env *env, emacs_value _delta)
{
    EGIT_ASSERT_DIFF_DELTA(_delta);
    git_diff_delta *delta = EGIT_EXTRACT(_delta);
    return EM_INTEGER(delta->similarity);
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
    int flags = EM_EQ(side, em_old) ? delta->old_file.flags : delta->new_file.flags;
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
    int num = EM_EQ(side, em_old) ? hunk->old_lines : hunk->new_lines;
    return EM_INTEGER(num);
}

EGIT_DOC(diff_hunk_start, "HUNK SIDE",
         "Return starting line number of HUNK.\n"
         "SIDE must be either `old' or `new'.");
emacs_value egit_diff_hunk_start(emacs_env *env, emacs_value _hunk, emacs_value side)
{
    EGIT_ASSERT_DIFF_HUNK(_hunk);
    git_diff_hunk *hunk = EGIT_EXTRACT(_hunk);
    int num = EM_EQ(side, em_old) ? hunk->old_start : hunk->new_start;
    return EM_INTEGER(num);
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
    return EM_INTEGER(line->origin);
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
    int lineno = EM_EQ(side, em_old) ? line->old_lineno : line->new_lineno;
    return EM_INTEGER(lineno);
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
    else if (EM_EQ(_type, em_unmodified))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_UNMODIFIED);
    else if (EM_EQ(_type, em_added))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_ADDED);
    else if (EM_EQ(_type, em_deleted))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_DELETED);
    else if (EM_EQ(_type, em_modified))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_MODIFIED);
    else if (EM_EQ(_type, em_renamed))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_RENAMED);
    else if (EM_EQ(_type, em_copied))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_COPIED);
    else if (EM_EQ(_type, em_ignored))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_IGNORED);
    else if (EM_EQ(_type, em_untracked))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_UNTRACKED);
    else if (EM_EQ(_type, em_typechange))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_TYPECHANGE);
    else if (EM_EQ(_type, em_unreadable))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_UNREADABLE);
    else if (EM_EQ(_type, em_conflicted))
        num = git_diff_num_deltas_of_type(diff, GIT_DELTA_CONFLICTED);
    else {
        em_signal_wrong_value(env, _type);
        return em_nil;
    }

    return EM_INTEGER(num);
}
