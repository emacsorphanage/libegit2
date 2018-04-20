#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


// =============================================================================
// Constructors

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

emacs_value egit_repository_commondir(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *path = git_repository_commondir(repo);
    return env->make_string(env, path, strlen(path));
}

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

emacs_value egit_repository_head(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    git_reference *ref;
    int retval = git_repository_head(&ref, repo);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_REFERENCE, ref);
}

emacs_value egit_repository_path(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *path = git_repository_path(repo);
    return env->make_string(env, path, strlen(path));
}

emacs_value egit_repository_state(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    git_repository_state_t state = git_repository_state(repo);
    switch (state) {
    case GIT_REPOSITORY_STATE_MERGE: return em_merge;
    case GIT_REPOSITORY_STATE_REVERT: return em_revert;
    case GIT_REPOSITORY_STATE_REVERT_SEQUENCE: return em_cherrypick_sequence;
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

emacs_value egit_repository_workdir(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    const char *path = git_repository_workdir(repo);
    return path ? env->make_string(env, path, strlen(path)) : em_nil;
}


// =============================================================================
// Predicates

emacs_value egit_repository_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_REPOSITORY ? em_t : em_nil;
}

emacs_value egit_repository_bare_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    return git_repository_is_bare(repo) ? em_t : em_nil;
}

emacs_value egit_repository_empty_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_is_empty(repo);
    EGIT_CHECK_ERROR(retval);
    return retval ? em_t : em_nil;
}

emacs_value egit_repository_head_detached_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_head_detached(repo);
    EGIT_CHECK_ERROR(retval);
    return retval ? em_t : em_nil;
}

emacs_value egit_repository_head_unborn_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_repository_head_unborn(repo);
    EGIT_CHECK_ERROR(retval);
    return retval ? em_t : em_nil;
}

emacs_value egit_repository_shallow_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    return git_repository_is_shallow(repo) ? em_t : em_nil;
}

emacs_value egit_repository_worktree_p(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    return git_repository_is_worktree(repo) ? em_t : em_nil;
}
