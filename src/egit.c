#include <stdlib.h>

#include "emacs-module.h"
#include "git2.h"
#include "uthash.h"

#include "interface.h"
#include "egit-reference.h"
#include "egit-repository.h"
#include "egit.h"

egit_object *object_store = NULL;

typedef emacs_value (*func_1)(emacs_env*, emacs_value);
typedef emacs_value (*func_2)(emacs_env*, emacs_value, emacs_value);

#define FUNC(target, min_nargs, max_nargs, arglist, docstring)          \
    env->make_function(                                                 \
        env, (min_nargs), (max_nargs),                                  \
        egit_dispatch_##max_nargs,                                      \
        docstring "\n\n(fn " arglist ")",                               \
        (target)                                                        \
    )

#define DEFUN(name, func) em_defun(env, (name), (func))

#define GET_SAFE(arglist, nargs, index) ((index) < (nargs) ? (arglist)[(index)] : em_nil)

egit_type egit_get_type(emacs_env *env, emacs_value _obj)
{
    if (!em_user_ptrp(env, _obj))
        return EGIT_UNKNOWN;
    egit_object *obj = (egit_object*)env->get_user_ptr(env, _obj);
    return obj->type;
}

bool egit_assert_type(emacs_env *env, emacs_value obj, egit_type type, emacs_value predicate)
{
    if (type == egit_get_type(env, obj))
        return true;
    em_signal_wrong_type(env, predicate, obj);
    return false;
}

void egit_decref_wrapped(void *obj)
{
    egit_object *wrapper;
    HASH_FIND_PTR(object_store, &obj, wrapper);
    egit_decref_wrapper(wrapper);
}

void egit_decref_wrapper(void *_obj)
{
    egit_object *obj = (egit_object*)_obj;
    obj->refcount--;

    if (obj->refcount != 0)
        return;

    HASH_DEL(object_store, obj);

    switch (obj->type) {
    case EGIT_REFERENCE: {
        git_repository *repo = git_reference_owner(obj->ptr);
        git_reference_free(obj->ptr);
        egit_decref_wrapped(repo);
        break;
    }
    case EGIT_REPOSITORY:
        git_repository_free(obj->ptr);
        break;
    case EGIT_UNKNOWN:
        break;
    }

    free(obj);
}

egit_object *egit_incref(egit_type type, void *obj)
{
    egit_object *retval;
    HASH_FIND_PTR(object_store, &obj, retval);
    if (retval)
        retval->refcount++;
    else {
        retval = (egit_object*)malloc(sizeof(egit_object));
        retval->type = type;
        retval->refcount = 1;
        retval->ptr = obj;
        HASH_ADD_PTR(object_store, ptr, retval);
    }

    return retval;
}

emacs_value egit_wrap(emacs_env* env, egit_type type, void* data)
{
    egit_object *obj = egit_incref(type, data);

    switch (type) {
    case EGIT_REFERENCE:
        egit_incref(EGIT_REPOSITORY, git_reference_owner(data));
        break;
    default:
        break;
    }

    return env->make_user_ptr(env, egit_decref_wrapper, obj);
}

emacs_value egit_dispatch_1(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_1 func = (func_1) data;
    return func(env, GET_SAFE(args, nargs, 0));
}

emacs_value egit_dispatch_2(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_2 func = (func_2) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1));
}

bool egit_dispatch_error(emacs_env *env, int retval)
{
    if (retval >= 0) return false;

    const git_error *err = giterr_last();
    if (!err) return false;

    em_signal_giterr(env, err->klass, err->message);
    return true;
}

void egit_init(emacs_env *env)
{
    // Clone
    DEFUN("git-clone", FUNC(egit_clone, 2, 2, "URL PATH", ""));

    // Reference
    DEFUN("git-reference-name", FUNC(egit_reference_name, 1, 1, "REF", ""));
    DEFUN("git-reference-owner", FUNC(egit_reference_owner, 1, 1, "REF", ""));
    DEFUN("git-reference-resolve", FUNC(egit_reference_resolve, 1, 1, "REF", ""));
    DEFUN("git-reference-target", FUNC(egit_reference_target, 1, 1, "REF", ""));

    DEFUN("git-reference-p", FUNC(egit_reference_p, 1, 1, "OBJ", ""));

    // Repository
    DEFUN("git-repository-init", FUNC(egit_repository_init, 1, 2, "PATH &optional IS-BARE", ""));
    DEFUN("git-repository-open", FUNC(egit_repository_open, 1, 1, "PATH", ""));
    DEFUN("git-repository-open-bare", FUNC(egit_repository_open_bare, 1, 1, "PATH", ""));

    DEFUN("git-repository-commondir", FUNC(egit_repository_commondir, 1, 1, "REPO", ""));
    DEFUN("git-repository-head", FUNC(egit_repository_head, 1, 1, "REPO", ""));
    DEFUN("git-repository-ident", FUNC(egit_repository_ident, 1, 1, "REPO", ""));
    DEFUN("git-repository-path", FUNC(egit_repository_path, 1, 1, "REPO", ""));
    DEFUN("git-repository-state", FUNC(egit_repository_state, 1, 1, "REPO", ""));
    DEFUN("git-repository-workdir", FUNC(egit_repository_workdir, 1, 1, "REPO", ""));

    DEFUN("git-repository-p", FUNC(egit_repository_p, 1, 1, "OBJ", ""));
    DEFUN("git-repository-bare-p", FUNC(egit_repository_bare_p, 1, 1, "REPO", ""));
    DEFUN("git-repository-empty-p", FUNC(egit_repository_empty_p, 1, 1, "REPO", ""));
    DEFUN("git-repository-head-detached-p", FUNC(egit_repository_empty_p, 1, 1, "REPO", ""));
    DEFUN("git-repository-head-unborn-p", FUNC(egit_repository_empty_p, 1, 1, "REPO", ""));
    DEFUN("git-repository-shallow-p", FUNC(egit_repository_shallow_p, 1, 1, "REPO", ""));
    DEFUN("git-repository-worktree-p", FUNC(egit_repository_worktree_p, 1, 1, "REPO", ""));
}
