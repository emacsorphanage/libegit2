#include <string.h>
#include "git2.h"

#include "egit-util.h"
#include "egit-status.h"
#include "egit.h"
#include "interface.h"

static int foreach_callback(const char *, unsigned int, void*);

static bool convert_show_option(git_status_show_t *, emacs_env *, emacs_value);
static bool convert_flags_option(git_status_opt_t *, emacs_env *, emacs_value);
static bool convert_baseline_option(git_tree **, emacs_env *, emacs_value);

EGIT_DOC(status_decode, "STATUS",
         "Decode git file STATUS.\n\n"
         "The return value is the same as that of `libgit-status-file'.");
emacs_value egit_status_decode(emacs_env *env, emacs_value status)
{
    intmax_t flags;
    emacs_value statuses[16];
    int nstatuses;

    EM_ASSERT_INTEGER(status);
    flags = EM_EXTRACT_INTEGER(status);

#define CHECK(name, symbol)                             \
    do {                                                \
        if (flags & GIT_STATUS_##name) {                \
            statuses[nstatuses++] = em_##symbol;        \
        }                                               \
    } while (false)

    nstatuses = 0;
    CHECK(INDEX_NEW, index_new);
    CHECK(INDEX_MODIFIED, index_modified);
    CHECK(INDEX_DELETED, index_deleted);
    CHECK(INDEX_RENAMED, index_renamed);
    CHECK(INDEX_TYPECHANGE, index_typechange);
    CHECK(WT_NEW, wt_new);
    CHECK(WT_MODIFIED, wt_modified);
    CHECK(WT_DELETED, wt_deleted);
    CHECK(WT_TYPECHANGE, wt_typechange);
    CHECK(WT_RENAMED, wt_renamed);
    CHECK(WT_UNREADABLE, wt_unreadable);
    CHECK(IGNORED, ignored);
    CHECK(CONFLICTED, conflicted);
#undef CHECK

    return em_list(env, statuses, nstatuses);
}

EGIT_DOC(status_file, "REPO PATH", "Get status of PATH in REPO.\n\n"
         "PATH is must be relative to REPO's root directory.\n"
         "The return value is a list of symbols describing status of a file:\n\n"
         "  `index-new'          - File is new in the index.\n"
         "  `index-modified'     - File is modified and added to the index.\n"
         "  `index-deleted'      - File is deleted in the index.\n"
         "  `index-renamed'      - File is renamed in the index.\n"
         "  `index-typechange'   - File type is changed in the index.\n"
         "  `wt-new'             - New file.\n"
         "  `wt-modified'        - File is modified in the worktree.\n"
         "  `wt-deleted'         - File is deleted in the worktree.\n"
         "  `wt-typechange'      - File type is changed in the worktree.\n"
         "  `wt-renamed'         - File is renamed in the worktree.\n"
         "  `wt-unreadable'      - File is unreadable.\n"
         "  `ignored'            - File is ignored.\n"
         "  `conflicted'         - File is in conflicted state.");
emacs_value egit_status_file(emacs_env *env, emacs_value _repo,
                             emacs_value _path)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_path);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *path = EM_EXTRACT_STRING(_path);

    unsigned int flags;
    int rv = git_status_file(&flags, repo, path);

    free(path);
    EGIT_CHECK_ERROR(rv);

    return egit_status_decode(env, EM_INTEGER(flags));
}

EGIT_DOC(status_should_ignore_p, "REPO PATH",
         "Return non-nil if the ignore rules would apply to PATH in REPO.\n\n"
         "PATH must be relative to the repository root directory.");
emacs_value egit_status_should_ignore_p(emacs_env *env, emacs_value _repo,
                                        emacs_value _path)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_path);

    char *path = EM_EXTRACT_STRING(_path);
    git_repository *repo = EGIT_EXTRACT(_repo);

    int ignored;
    int rv = git_status_should_ignore(&ignored, repo, path);

    free(path);
    EGIT_CHECK_ERROR(rv);

    return ignored == 0 ? em_nil : em_t;
}

bool convert_show_option(git_status_show_t *out, emacs_env *env,
                         emacs_value arg)
{
    if (EM_EQ(arg, em_index_only)) {
        *out = GIT_STATUS_SHOW_INDEX_ONLY;
        return true;
    } else if (EM_EQ(arg, em_workdir_only)) {
        *out = GIT_STATUS_SHOW_WORKDIR_ONLY;
        return true;
    } else if (EM_EQ(arg, em_index_and_workdir)) {
        *out = GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
        return true;
    } else if (EM_EXTRACT_BOOLEAN(arg)) {
        em_signal_wrong_value(env, arg);
        return false;
    }
    *out = GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
    return true;
}

bool convert_flags_option(git_status_opt_t *out, emacs_env *env,
                          emacs_value arg)
{
    if (!EM_EXTRACT_BOOLEAN(arg)) {
        *out = GIT_STATUS_OPT_DEFAULTS;
        return true;
    }

    if (em_assert_list(env, em_nil, arg) < 0)
        return false;

    *out = 0;
    emacs_value flag;
    while (EM_EXTRACT_BOOLEAN(arg)) {
        flag = em_car(env, arg);
        arg = em_cdr(env, arg);

#define CHECK(symbol, enum)                                     \
        if (EM_EQ(flag, em_##symbol)) {                  \
            *out |= GIT_STATUS_OPT_##enum;                      \
            continue;                                           \
        }

        CHECK(include_untracked, INCLUDE_UNTRACKED);
        CHECK(include_ignored, INCLUDE_IGNORED);
        CHECK(include_unmodified, INCLUDE_UNMODIFIED);
        CHECK(exclude_submodules, EXCLUDE_SUBMODULES);
        CHECK(recurse_untracked_dirs, RECURSE_UNTRACKED_DIRS);
        CHECK(disable_pathspec_match, DISABLE_PATHSPEC_MATCH);
        CHECK(recurse_ignored_dirs, RECURSE_IGNORED_DIRS);
        CHECK(renames_head_to_index, RENAMES_HEAD_TO_INDEX);
        CHECK(renames_index_to_workdir, RENAMES_INDEX_TO_WORKDIR);
        CHECK(sort_case_sensitively, SORT_CASE_SENSITIVELY);
        CHECK(sort_case_insensitively, SORT_CASE_INSENSITIVELY);
        CHECK(renames_from_rewrites, RENAMES_FROM_REWRITES);
        CHECK(no_refresh, NO_REFRESH);
        CHECK(update_index, UPDATE_INDEX);
        CHECK(include_unreadable, INCLUDE_UNREADABLE);
        CHECK(include_unreadable_as_untracked, INCLUDE_UNREADABLE_AS_UNTRACKED);
#undef CHECK

        em_signal_wrong_value(env, flag);
        return false;
    }

    return true;
}

bool convert_baseline_option(git_tree **out, emacs_env *env, emacs_value arg)
{
    if (!EM_EXTRACT_BOOLEAN(arg)) {
        *out = NULL;
        return true;
    }
    if (!egit_assert_type(env, arg, EGIT_TREE, em_libgit_tree_p)) {
        return false;
    }

    *out = EGIT_EXTRACT(arg);
    return true;
}

EGIT_DOC(status_foreach, "REPO FUNCTION &optional SHOW FLAGS PATHSPEC BASELINE",
         "Gather file statuses in REPO and call FUNCTION for each one.\n\n"
         "FUNCTION is called with two arguments: FILE and STATUS.\n"
         "FILE is path to a file, relative to the root directory.\n"
         "STATUS is an object describing the file status.\n"
         "Use `libgit-status-decode' to decode it.\n\n"
         "SHOW is a symbol which controls which files to show.  Possible "
         "values are:\n\n"
         "  `index-only'        - status based on HEAD to index comparison,\n"
         "                        not looking at working directory change\n\n"
         "  `workdir-only'      - status based on index to working directory\n"
         "                        comparison, not comparing the index to the HEAD\n\n"
         "  `index-and-workdir' - the default. Roughly matches `git status --porcelain`\n"
         "                        regarding which files are included and in what order.\n\n"
         "FLAGS is a list of symbols which control status callbacks:\n\n"
         "  `include-untracked' says that callbacks should be made\n"
         "    on untracked files.  These will only be made if the workdir files are\n"
         "    included in the SHOW argument.\n\n"
         "  `include-ignored' says that ignored files get callbacks.\n"
         "    Again, these callbacks will only be made if the workdir files are\n"
         "    included in the SHOW argument.\n\n"
         "  `include-unmodified' indicates that callback should be\n"
         "    made even on unmodified files.\n\n"
         "  `exclude-submodules' indicates that submodules should be\n"
         "    skipped.  This only applies if there are no pending typechanges to\n"
         "    the submodule (either from or to another type).\n\n"
         "  `recurse-untracked-dirs' indicates that all files in\n"
         "    untracked directories should be included.  Normally if an entire\n"
         "    directory is new, then just the top-level directory is included (with\n"
         "    a trailing slash on the entry name). This flag says to include all\n"
         "    of the individual files in the directory instead.\n\n"
         "  `disable-pathspec-match' indicates that the given path\n"
         "    should be treated as a literal path, and not as a pathspec pattern.\n\n"
         "  `recurse-ignored-dirs' indicates that the contents of\n"
         "    ignored directories should be included in the status.  This is like\n"
         "    doing `git ls-files -o -i --exclude-standard` with core git.\n\n"
         "  `renames-head-to-index' indicates that rename detection\n"
         "    should be processed between the head and the index and enables\n"
         "    the `index-renamed' as a possible status symbol.\n\n"
         "  `renames-index-to-workdir' indicates that rename\n"
         "    detection should be run between the index and the working directory\n"
         "    and enables `wt-renamed' as a possible status symbol.\n\n"
         "  `sort-case-sensitively' overrides the native case\n"
         "    sensitivity for the file system and forces the output to be in\n"
         "    case-sensitive order\n\n"
         "  `sort-case-insensitively' overrides the native case\n"
         "    sensitivity for the file system and forces the output to be in\n"
         "    case-insensitive order\n\n"
         "  `renames-from-rewrites' indicates that rename detection\n"
         "    should include rewritten files\n\n"
         "  `no-refresh' bypasses the default status behavior of\n"
         "    doing a 'soft' index reload (i.e. reloading the index data if the\n"
         "    file on disk has been modified outside libgit2).\n\n"
         "  `update-index' tells libgit2 to refresh the stat cache\n"
         "    in the index for files that are unchanged but have out of date stat\n"
         "    information in the index.  It will result in less work being done on\n"
         "    subsequent calls to get status.  This is mutually exclusive with the\n"
         "    `no-refresh' option.\n\n"
         "PATHSPEC is list of path patterns to match (using fnmatch-style matching),\n"
         "or just a list of paths to match exactly if `disable-pathspec-match' is\n"
         "given in FLAGS.\n\n"
         "BASELINE is the tree to be used for comparison to the working directory and\n"
         "index; defaults to HEAD."
    );
emacs_value egit_status_foreach(emacs_env *env, emacs_value _repo,
                                emacs_value function, emacs_value show,
                                emacs_value flags, emacs_value pathspec,
                                emacs_value baseline)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_FUNCTION(function);

    git_status_options options;
    git_status_init_options(&options, GIT_STATUS_OPTIONS_VERSION);

    if (!convert_show_option(&options.show, env, show)) {
        return em_nil;
    }
    if (!convert_flags_option(&options.flags, env, flags)) {
        return em_nil;
    }
    if (!egit_strarray_from_list(&options.pathspec, env, pathspec)) {
        return em_nil;
    }
    if (!convert_baseline_option(&options.baseline, env, baseline)) {
        egit_strarray_dispose(&options.pathspec);
        return em_nil;
    }

    git_repository *repo = EGIT_EXTRACT(_repo);
    egit_generic_payload ctx = {.env = env, .func = function};

    int rv = git_status_foreach_ext(repo, &options, &foreach_callback, (void *)(&ctx));
    egit_strarray_dispose(&options.pathspec);

    if (rv != GIT_EUSER) {
        EGIT_CHECK_ERROR(rv);
    }

    return em_nil;
}

int foreach_callback(const char *path, unsigned int flags, void *payload)
{
    egit_generic_payload *ctx;
    emacs_env *env;
    emacs_value function;
    emacs_value args[2];

    ctx = (egit_generic_payload *)payload;
    env = ctx->env;
    function = ctx->func;

    args[0] = EM_STRING(path);
    args[1] = EM_INTEGER(flags);
    env->funcall(env, function, 2, args);

    EM_RETURN_IF_NLE(GIT_EUSER);

    return 0;
}
