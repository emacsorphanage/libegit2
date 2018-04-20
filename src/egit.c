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

bool egit_assert_object(emacs_env *env, emacs_value obj)
{
    egit_type type = egit_get_type(env, obj);
    if (type == EGIT_COMMIT || type == EGIT_TREE ||
        type == EGIT_BLOB || type == EGIT_TAG || type == EGIT_OBJECT)
        return true;
    em_signal_wrong_type(env, em_git_object_p, obj);
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
    case EGIT_COMMIT:
    case EGIT_TREE:
    case EGIT_BLOB:
    case EGIT_TAG:
    case EGIT_OBJECT: {
        git_repository *repo = git_object_owner(obj->ptr);
        git_object_free(obj->ptr);
        egit_decref_wrapped(repo);
        break;
    }
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
    // If it's a git_object, try to be more specific
    if (type == EGIT_OBJECT) {
        switch (git_object_type(data)) {
        case GIT_OBJ_COMMIT: type = EGIT_COMMIT; break;
        case GIT_OBJ_TREE: type = EGIT_TREE; break;
        case GIT_OBJ_BLOB: type = EGIT_BLOB; break;
        case GIT_OBJ_TAG: type = EGIT_TAG; break;
        default: break;
        }
    }

    egit_object *obj = egit_incref(type, data);

    switch (type) {
    case EGIT_COMMIT:
    case EGIT_TREE:
    case EGIT_BLOB:
    case EGIT_TAG:
    case EGIT_OBJECT:
        egit_incref(EGIT_REPOSITORY, git_object_owner(data));
        break;
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

#define DEFUN(ename, cname, min_nargs, max_nargs)                       \
    em_defun(env, (ename),                                              \
             env->make_function(                                        \
                 env, (min_nargs), (max_nargs),                         \
                 egit_dispatch_##max_nargs,                             \
                 egit_##cname##__doc,                                   \
                 egit_##cname))

void egit_init(emacs_env *env)
{
    // Clone
    DEFUN("git-clone", clone, 2, 2);

    // Reference
    DEFUN("git-reference-name", reference_name, 1, 1);
    DEFUN("git-reference-owner", reference_owner, 1, 1);
    DEFUN("git-reference-resolve", reference_resolve, 1, 1);
    DEFUN("git-reference-target", reference_target, 1, 1);

    DEFUN("git-reference-p", reference_p, 1, 1);

    // Repository
    DEFUN("git-repository-init", repository_init, 1, 2);
    DEFUN("git-repository-open", repository_open, 1, 1);
    DEFUN("git-repository-open-bare", repository_open_bare, 1, 1);

    DEFUN("git-repository-commondir", repository_commondir, 1, 1);
    DEFUN("git-repository-get-namespace", repository_get_namespace, 1, 1);
    DEFUN("git-repository-head", repository_head, 1, 1);
    DEFUN("git-repository-head-for-worktree", repository_head_for_worktree, 2, 2);
    DEFUN("git-repository-ident", repository_ident, 1, 1);
    DEFUN("git-repository-message", repository_message, 1, 1);
    DEFUN("git-repository-path", repository_path, 1, 1);
    DEFUN("git-repository-state", repository_state, 1, 1);
    DEFUN("git-repository-workdir", repository_workdir, 1, 1);

    DEFUN("git-repository-detach-head", repository_detach_head, 1, 1);
    DEFUN("git-repository-message-remove", repository_message_remove, 1, 1);

    DEFUN("git-repository-p", repository_p, 1, 1);
    DEFUN("git-repository-bare-p", repository_bare_p, 1, 1);
    DEFUN("git-repository-empty-p", repository_empty_p, 1, 1);
    DEFUN("git-repository-head-detached-p", repository_empty_p, 1, 1);
    DEFUN("git-repository-head-unborn-p", repository_empty_p, 1, 1);
    DEFUN("git-repository-shallow-p", repository_shallow_p, 1, 1);
    DEFUN("git-repository-worktree-p", repository_worktree_p, 1, 1);
}
