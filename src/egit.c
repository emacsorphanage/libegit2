#include <stdlib.h>

#include "emacs-module.h"
#include "git2.h"
#include "uthash.h"

#include "interface.h"
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

static void egit_free(void* _obj)
{
    egit_object *obj = (egit_object*) _obj;
    obj->refcount--;

    if (obj->refcount != 0)
        return;

    HASH_DEL(object_store, obj);

    switch (obj->type) {
    case EGIT_REPOSITORY:
        git_repository_free(obj->ptr);
        break;
    case EGIT_UNKNOWN:
        break;
    }

    free(obj);
}

emacs_value egit_wrap(emacs_env* env, egit_type type, void* data)
{
    egit_object *obj;
    HASH_FIND_PTR(object_store, &data, obj);
    if (obj)
        obj->refcount++;
    else {
        obj = (egit_object*)malloc(sizeof(egit_object));
        obj->type = type;
        obj->refcount = 1;
        obj->ptr = data;
        HASH_ADD_PTR(object_store, ptr, obj);
    }

    return env->make_user_ptr(env, egit_free, obj);
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
    if (retval == 0) return false;

    const git_error *err = giterr_last();
    if (!err) return false;

    em_signal_giterr(env, err->klass, err->message);
    return true;
}

void egit_init(emacs_env *env)
{
    // clone
    DEFUN("git-clone", FUNC(egit_clone, 2, 2, "URL PATH", ""));

    // repository
    DEFUN("git-repository-p", FUNC(egit_repository_p, 1, 1, "OBJ", ""));
    DEFUN("git-repository-init", FUNC(egit_repository_init, 1, 2, "PATH &optional IS-BARE", ""));
    DEFUN("git-repository-open", FUNC(egit_repository_open, 1, 1, "PATH", ""));
    DEFUN("git-repository-path", FUNC(egit_repository_path, 1, 1, "REPO", ""));
    DEFUN("git-repository-workdir", FUNC(egit_repository_workdir, 1, 1, "REPO", ""));
}
