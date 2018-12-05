#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


// =============================================================================
// Constructors

EGIT_DOC(repository_init, "PATH &optional IS-BARE",
         "Initialize a repository at PATH and return it.\n"
         "If IS-BARE then create a bare repository.");
emacs_value egit_repository_init(emacs_env *env, emacs_value _path, emacs_value _is_bare)
{
    EM_ASSERT_STRING(_path);
    EM_NORMALIZE_PATH(_path);

    git_repository *repo;
    unsigned int is_bare = EM_EXTRACT_BOOLEAN(_is_bare);
    int retval;
    {
        char *path = EM_EXTRACT_STRING(_path);
        retval = git_repository_init(&repo, path, is_bare);
        free(path);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REPOSITORY, repo, NULL);
}

EGIT_DOC(repository_open, "PATH", "Open an existing repository at PATH.");
emacs_value egit_repository_open(emacs_env *env, emacs_value _path)
{
    EM_ASSERT_STRING(_path);
    EM_NORMALIZE_PATH(_path);

    git_repository *repo;
    int retval;
    {
        char *path = EM_EXTRACT_STRING(_path);
        retval = git_repository_open(&repo, path);
        free(path);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REPOSITORY, repo, NULL);
}

EGIT_DOC(repository_open_bare, "PATH",
         "Open an existing bare repository at PATH.\n"
         "This is faster than `git-repository-open'.");
emacs_value egit_repository_open_bare(emacs_env *env, emacs_value _path)
{
    EM_ASSERT_STRING(_path);
    EM_NORMALIZE_PATH(_path);

    git_repository *repo;
    int retval;
    {
        char *path = EM_EXTRACT_STRING(_path);
        retval = git_repository_open_bare(&repo, path);
        free(path);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REPOSITORY, repo, NULL);
}


// =============================================================================
// Getters

EGIT_DOC(repository_commondir, "REPO", "Return the shared common directory for REPO.");
emacs_value egit_repository_commondir(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *path = git_repository_commondir(repo);
    emacs_value retval = EM_STRING(path);
    EM_NORMALIZE_PATH(retval);
    return retval;
}

EGIT_DOC(repository_config, "REPO", "Return the git config associated with REPO.");
emacs_value egit_repository_config(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    git_config *config;
    int retval = git_repository_config(&config, repo);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CONFIG, config, NULL);
}

EGIT_DOC(repository_get_namespace, "REPO",
         "Return the currently active namespace for REPO, or nil.");
emacs_value egit_repository_get_namespace(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *namespace = git_repository_get_namespace(repo);
    return namespace ? EM_STRING(namespace) : esym_nil;
}

EGIT_DOC(repository_head, "REPO",
         "Retrieve and resolve the reference pointed at by HEAD in REPO.");
emacs_value egit_repository_head(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    git_reference *ref;
    int retval = git_repository_head(&ref, repo);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_REFERENCE, ref, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(repository_head_for_worktree, "REPO NAME",
         "Return the HEAD for the worktree named NAME at REPO.");
emacs_value egit_repository_head_for_worktree(emacs_env *env, emacs_value _repo, emacs_value _name)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_reference *ref;
    int retval;
    {
        char *name = EM_EXTRACT_STRING(_name);
        retval = git_repository_head_for_worktree(&ref, repo, name);
        free(name);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(repository_ident, "REPO",
         "Return the configured identity to use for reflogs in REPO.\n"
         "The return value is a cons cell (name . email).");
emacs_value egit_repository_ident(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *name, *email;
    int retval = git_repository_ident(&name, &email, repo);
    EGIT_CHECK_ERROR(retval);
    emacs_value _name = name ? EM_STRING(name) : esym_nil;
    emacs_value _email = email ? EM_STRING(email) : esym_nil;
    return em_cons(env, _name, _email);
}

EGIT_DOC(repository_index, "REPO", "Return the index for REPO.");
emacs_value egit_repository_index(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    git_index *index;
    int retval = git_repository_index(&index, repo);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_INDEX, index, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(repository_message, "REPO",
         "Return the prepared commit message for REPO, or nil.\n"
         "This is the contents of .git/MERGE_MSG.");
emacs_value egit_repository_message(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    git_buf buf = {NULL, 0, 0};
    int retval = git_repository_message(&buf, repo);
    if (retval == GIT_ENOTFOUND) return esym_nil;
    EGIT_CHECK_ERROR(retval);
    EGIT_RET_BUF_AS_STRING(buf);
}

EGIT_DOC(repository_path, "REPO",
         "Return the path to REPO.\n"
         "This is the path to the .git folder for normal repositories.");
emacs_value egit_repository_path(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *path = git_repository_path(repo);
    emacs_value retval = EM_STRING(path);
    EM_NORMALIZE_PATH(retval);
    return retval;
}

EGIT_DOC(repository_state, "REPO",
         "Return the current state of REPO, indicating whether an operation is in progress.\n"
         "Possible return values are:\n"
         "  - nil\n"
         "  - `merge'\n"
         "  - `revert'\n"
         "  - `revert-sequence'\n"
         "  - `cherrypick'\n"
         "  - `cherrypick-sequence'\n"
         "  - `bisect'\n"
         "  - `rebase'\n"
         "  - `rebase-interactive'\n"
         "  - `rebase-merge'\n"
         "  - `apply-mailbox'\n"
         "  - `apply-mailbox-or-rebase'");
emacs_value egit_repository_state(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    git_repository_state_t state = git_repository_state(repo);
    return em_findenum_repository_state(state);
}

EGIT_DOC(repository_workdir, "REPO", "Return the path to the working directory of REPO, or nil.");
emacs_value egit_repository_workdir(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *path = git_repository_workdir(repo);
    if (!path)
        return esym_nil;
    emacs_value retval = EM_STRING(path);
    EM_NORMALIZE_PATH(retval);
    return retval;
}


// =============================================================================
// Operations

EGIT_DOC(repository_detach_head, "REPO",
         "Detach the head of REPO.\n\n"
         "If HEAD is already detached and points to a commit, do nothing.\n"
         "If HEAD is already detached and points to a non-commitish, error.\n"
         "Otherwise, update HEAD, making it point to the peeled commit.");
emacs_value egit_repository_detach_head(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_detach_head(repo);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(repository_message_remove, "REPO",
         "Remove the stored commit message of REPO.\n"
         "This deletes the file .git/MERGE_MSG.  It is an error if the file does not exist.");
emacs_value egit_repository_message_remove(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_message_remove(repo);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(repository_set_head, "REPO REFNAME",
         "Make the head of REPO point to the specified reference.");
emacs_value egit_repository_set_head(emacs_env *env, emacs_value _repo, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval;
    {
        char *refname = EM_EXTRACT_STRING(_refname);
        retval = git_repository_set_head(repo, refname);
        free(refname);
    }
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(repository_set_head_detached, "REPO COMMITISH",
         "Make the head of REPO point directly to the commit.");
emacs_value egit_repository_set_head_detached(emacs_env *env, emacs_value _repo, emacs_value _commitish)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_commitish);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid commitish;
    EGIT_EXTRACT_OID(_commitish, commitish);

    int retval = git_repository_set_head_detached(repo, &commitish);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(repository_set_ident, "REPO &optional NAME EMAIL",
         "Set the identity to be used for writing reflogs.");
emacs_value egit_repository_set_ident(
    emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _email)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING_OR_NIL(_name);
    EM_ASSERT_STRING_OR_NIL(_email);

    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval;
    {
        char *name = EM_EXTRACT_STRING_OR_NULL(_name);
        char *email = EM_EXTRACT_STRING_OR_NULL(_email);
        retval = git_repository_set_ident(repo, name, email);
        free(name);
        free(email);
    }
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(repository_set_namespace, "REPO NAMESPACE",
         "Sets the active namespace for this Git Repository.");
emacs_value egit_repository_set_namespace(
    emacs_env *env, emacs_value _repo, emacs_value _nmspace)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_nmspace);

    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval;
    {
        char *nmspace = EM_EXTRACT_STRING(_nmspace);
        retval = git_repository_set_namespace(repo, nmspace);
        free(nmspace);
    }
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(repository_set_workdir, "REPO WORKDIR &optional UPDATE-GITLINK",
         "Set the path to the working directory for REPO to WORKDIR.");
emacs_value egit_repository_set_workdir(
    emacs_env *env, emacs_value _repo, emacs_value _workdir, emacs_value _update_gitlink)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_workdir);
    EM_NORMALIZE_PATH(_workdir);

    git_repository *repo = EGIT_EXTRACT(_repo);
    int update_gitlink = EM_EXTRACT_BOOLEAN(_update_gitlink);
    int retval;
    {
        char *workdir = EM_EXTRACT_STRING(_workdir);
        retval = git_repository_set_workdir(repo, workdir, update_gitlink);
        free(workdir);
    }
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(repository_state_cleanup, "REPO",
         "Remove all the metadata in REPO associated with an ongoing\n"
         "command like merge, revert, cherry-pick, etc.");
emacs_value egit_repository_state_cleanup(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_state_cleanup(repo);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}


// =============================================================================
// Predicates

EGIT_DOC(repository_bare_p, "REPO", "Return non-nil if REPO is a bare repository.");
emacs_value egit_repository_bare_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    return git_repository_is_bare(repo) ? esym_t : esym_nil;
}

EGIT_DOC(repository_empty_p, "REPO", "Return non-nil if REPO is empty.");
emacs_value egit_repository_empty_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_is_empty(repo);
    EGIT_CHECK_ERROR(retval);
    return retval ? esym_t : esym_nil;
}

EGIT_DOC(repository_head_detached_p, "REPO", "Return non-nil if REPO is in detached head state.");
emacs_value egit_repository_head_detached_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_head_detached(repo);
    EGIT_CHECK_ERROR(retval);
    return retval ? esym_t : esym_nil;
}

EGIT_DOC(repository_head_unborn_p, "REPO", "Return non-nil if REPO's HEAD is unborn.");
emacs_value egit_repository_head_unborn_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_head_unborn(repo);
    EGIT_CHECK_ERROR(retval);
    return retval ? esym_t : esym_nil;
}

EGIT_DOC(repository_shallow_p, "REPO", "Return non-nil if REPO was a shallow clone.");
emacs_value egit_repository_shallow_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    return git_repository_is_shallow(repo) ? esym_t : esym_nil;
}

EGIT_DOC(repository_worktree_p, "REPO", "Return non-nil if REPO is a linked worktree.");
emacs_value egit_repository_worktree_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    return git_repository_is_worktree(repo) ? esym_t : esym_nil;
}


// =============================================================================
// Miscellaneous

EGIT_DOC(repository_discover, "&optional PATH ACROSS-FS CEILING-DIRS",
         "Look for a git repository in PATH and return its path.\n"
         "If ACROSS-FS is nil, lookup will stop when a filesystem change is detected.\n"
         "CEILING-DIRS is a list of paths where lookup will stop.");
emacs_value egit_repository_discover(emacs_env *env, emacs_value _path, emacs_value _across_fs, emacs_value _ceiling_dirs)
{
    EM_ASSERT_STRING_OR_NIL(_path);

    // Check that _ceiling_dirs is a list of strings, and get the total length
    // of the buffer we need, including separators
    ptrdiff_t totsize = 0, size;
    {
        EM_DOLIST(car, _ceiling_dirs, count);
        EM_ASSERT_STRING(car);
        if (totsize > 0) totsize++;          // Space for the separator
        env->copy_string_contents(env, car, NULL, &size);
        totsize += size - 1;                 // Ignore the terminating null character
        EM_DOLIST_END(count);
    }

    // Allocate a buffer with the right size, and copy the string contents
    char *ceiling_dirs = (char*) malloc((totsize + 1) * sizeof(char));
    char *next = ceiling_dirs;
    {
        EM_DOLIST(car, _ceiling_dirs, copy);
        if (next != ceiling_dirs)
            *(next++) = GIT_PATH_LIST_SEPARATOR;
        env->copy_string_contents(env, car, NULL, &size);
        env->copy_string_contents(env, car, next, &size);
        next += size - 1;
        EM_DOLIST_END(copy);
    }
    *next = '\0';

    char *path;
    if (EM_EXTRACT_BOOLEAN(_path)) {
        EM_NORMALIZE_PATH(_path);
        path = EM_EXTRACT_STRING(_path);
    }
    else
        path = em_default_directory(env);

    int across_fs = EM_EXTRACT_BOOLEAN(_across_fs);

    git_buf out = {0};
    int retval = git_repository_discover(&out, path, across_fs, ceiling_dirs);
    free(path);
    free(ceiling_dirs);
    EGIT_CHECK_ERROR(retval);

    emacs_value ret = env->make_string(env, out.ptr, out.size);
    EM_NORMALIZE_PATH(ret);

    git_buf_dispose(&out);
    return ret;
}
