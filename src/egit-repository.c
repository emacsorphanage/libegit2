#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"

static bool _egit_repository_p(emacs_env *env, emacs_value _obj)
{
    if (!em_user_ptrp(env, _obj))
        return false;
    egit_object *obj = (egit_object*)env->get_user_ptr(env, _obj);
    return obj->type == EGIT_REPOSITORY;
}

emacs_value egit_repository_p(emacs_env *env, emacs_value obj)
{
    return _egit_repository_p(env, obj) ? em_t : em_nil;
}

emacs_value egit_clone(emacs_env *env, emacs_value _url, emacs_value _path)
{
    if (!em_assert(env, em_stringp, _url)) return em_nil;
    if (!em_assert(env, em_stringp, _path)) return em_nil;

    git_repository *repo;
    int retval;
    {
        char *url = em_get_string(env, _url);
        char *path = em_get_string(env, _path);
        retval = git_clone(&repo, url, path, NULL);
        free(url);
        free(path);
    }
    if (egit_dispatch_error(env, retval)) return em_nil;

    return egit_wrap(env, EGIT_REPOSITORY, repo);
}

emacs_value egit_repository_init(emacs_env *env, emacs_value _path, emacs_value _is_bare)
{
    if (!em_assert(env, em_stringp, _path)) return em_nil;

    git_repository *repo;
    int retval;
    {
        char *path = em_get_string(env, _path);
        unsigned int is_bare = env->is_not_nil(env, _is_bare) ? 1 : 0;
        retval = git_repository_init(&repo, path, is_bare);
        free(path);
    }
    if (egit_dispatch_error(env, retval)) return em_nil;

    return egit_wrap(env, EGIT_REPOSITORY, repo);
}

emacs_value egit_repository_open(emacs_env *env, emacs_value _path)
{
    if (!em_assert(env, em_stringp, _path)) return em_nil;

    git_repository *repo;
    int retval;
    {
        char *path = em_get_string(env, _path);
        retval = git_repository_open(&repo, path);
        free(path);
    }
    if (egit_dispatch_error(env, retval)) return em_nil;

    return egit_wrap(env, EGIT_REPOSITORY, repo);
}
