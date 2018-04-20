#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


// =============================================================================
// Constructors

EGIT_DOC(clone, "URL PATH", "Clone the repository at URL to PATH and return it.");
emacs_value egit_clone(emacs_env *env, emacs_value _url, emacs_value _path)
{
    EGIT_ASSERT_STRING(_url);
    EGIT_ASSERT_STRING(_path);

    git_repository *repo;
    int retval;
    {
        char *url = em_get_string(env, _url);
        char *path = em_get_string(env, _path);
        retval = git_clone(&repo, url, path, NULL);
        free(url);
        free(path);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REPOSITORY, repo);
}

EGIT_DOC(repository_init, "PATH &optional IS-BARE",
         "Initialize a repository at PATH and return it.\n"
         "If IS-BARE then create a bare repository.");
emacs_value egit_repository_init(emacs_env *env, emacs_value _path, emacs_value _is_bare)
{
    EGIT_ASSERT_STRING(_path);

    git_repository *repo;
    int retval;
    {
        char *path = em_get_string(env, _path);
        unsigned int is_bare = env->is_not_nil(env, _is_bare) ? 1 : 0;
        retval = git_repository_init(&repo, path, is_bare);
        free(path);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REPOSITORY, repo);
}

EGIT_DOC(repository_open, "PATH", "Open an existing repository at PATH.");
emacs_value egit_repository_open(emacs_env *env, emacs_value _path)
{
    EGIT_ASSERT_STRING(_path);

    git_repository *repo;
    int retval;
    {
        char *path = em_get_string(env, _path);
        retval = git_repository_open(&repo, path);
        free(path);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REPOSITORY, repo);
}

EGIT_DOC(repository_open_bare, "PATH",
         "Open an existing bare repository at PATH.\n"
         "This is faster than `git-repository-open'.");
emacs_value egit_repository_open_bare(emacs_env *env, emacs_value _path)
{
    EGIT_ASSERT_STRING(_path);

    git_repository *repo;
    int retval;
    {
        char *path = em_get_string(env, _path);
        retval = git_repository_open_bare(&repo, path);
        free(path);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REPOSITORY, repo);
}


// =============================================================================
// Getters

EGIT_DOC(repository_commondir, "REPO", "Return the shared common directory for REPO.");
emacs_value egit_repository_commondir(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *path = git_repository_commondir(repo);
    return env->make_string(env, path, strlen(path));
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
    emacs_value _name = name ? env->make_string(env, name, strlen(name)) : em_nil;
    emacs_value _email = email ? env->make_string(env, email, strlen(email)) : em_nil;
    return em_cons(env, _name, _email);
}

EGIT_DOC(repository_get_namespace, "REPO",
         "Return the currently active namespace for REPO, or nil.");
emacs_value egit_repository_get_namespace(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *namespace = git_repository_get_namespace(repo);
    return namespace ? env->make_string(env, namespace, strlen(namespace)) : em_nil;
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
    return egit_wrap(env, EGIT_REFERENCE, ref);
}

EGIT_DOC(repository_head_for_worktree, "REPO NAME",
         "Return the HEAD for the worktree named NAME at REPO.");
emacs_value egit_repository_head_for_worktree(emacs_env *env, emacs_value _repo, emacs_value _name)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_name);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_reference *ref;
    int retval;
    {
        char *name = em_get_string(env, _name);
        retval = git_repository_head_for_worktree(&ref, repo, name);
        free(name);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref);
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
    if (retval == GIT_ENOTFOUND) return em_nil;
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
    return env->make_string(env, path, strlen(path));
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
    switch (state) {
    case GIT_REPOSITORY_STATE_MERGE: return em_merge;
    case GIT_REPOSITORY_STATE_REVERT: return em_revert;
    case GIT_REPOSITORY_STATE_REVERT_SEQUENCE: return em_revert_sequence;
    case GIT_REPOSITORY_STATE_CHERRYPICK: return em_cherrypick;
    case GIT_REPOSITORY_STATE_CHERRYPICK_SEQUENCE: return em_cherrypick_sequence;
    case GIT_REPOSITORY_STATE_BISECT: return em_bisect;
    case GIT_REPOSITORY_STATE_REBASE: return em_rebase;
    case GIT_REPOSITORY_STATE_REBASE_INTERACTIVE: return em_rebase_interactive;
    case GIT_REPOSITORY_STATE_REBASE_MERGE: return em_rebase_merge;
    case GIT_REPOSITORY_STATE_APPLY_MAILBOX: return em_apply_mailbox;
    case GIT_REPOSITORY_STATE_APPLY_MAILBOX_OR_REBASE: return em_apply_mailbox_or_rebase;
    default: return em_nil;
    }
}

EGIT_DOC(repository_workdir, "REPO", "Return the path to the working directory of REPO, or nil.");
emacs_value egit_repository_workdir(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *path = git_repository_workdir(repo);
    return path ? env->make_string(env, path, strlen(path)) : em_nil;
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
    return em_nil;
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
    return em_nil;
}


// =============================================================================
// Predicates

EGIT_DOC(repository_p, "OBJ", "Return non-nil if OBJ is a git repository.");
emacs_value egit_repository_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_REPOSITORY ? em_t : em_nil;
}

EGIT_DOC(repository_bare_p, "REPO", "Return non-nil if REPO is a bare repository.");
emacs_value egit_repository_bare_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    return git_repository_is_bare(repo) ? em_t : em_nil;
}

EGIT_DOC(repository_empty_p, "REPO", "Return non-nil if REPO is empty.");
emacs_value egit_repository_empty_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_is_empty(repo);
    EGIT_CHECK_ERROR(retval);
    return retval ? em_t : em_nil;
}

EGIT_DOC(repository_head_detached_p, "REPO", "Return non-nil if REPO is in detached head state.");
emacs_value egit_repository_head_detached_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_head_detached(repo);
    EGIT_CHECK_ERROR(retval);
    return retval ? em_t : em_nil;
}

EGIT_DOC(repository_head_unborn_p, "REPO", "Return non-nil if REPO's HEAD is unborn.");
emacs_value egit_repository_head_unborn_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_head_unborn(repo);
    EGIT_CHECK_ERROR(retval);
    return retval ? em_t : em_nil;
}

EGIT_DOC(repository_shallow_p, "REPO", "Return non-nil if REPO was a shallow clone.");
emacs_value egit_repository_shallow_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    return git_repository_is_shallow(repo) ? em_t : em_nil;
}

EGIT_DOC(repository_worktree_p, "REPO", "Return non-nil if REPO is a linked worktree.");
emacs_value egit_repository_worktree_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    return git_repository_is_worktree(repo) ? em_t : em_nil;
}