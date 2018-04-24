#include <stdlib.h>
#include <assert.h>

#include "emacs-module.h"
#include "git2.h"
#include "uthash.h"

#include "interface.h"
#include "egit-clone.h"
#include "egit-object.h"
#include "egit-reference.h"
#include "egit-repository.h"
#include "egit-revparse.h"
#include "egit.h"

// Hash table of stored objects
egit_object *object_store = NULL;

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
    em_signal_wrong_type(env, em_libgit_object_p, obj);
    return false;
}

void egit_decref_repository(git_repository *wrapped)
{
    // Look up the wrapper struct in the hash table, and call egit_decref_wrapper
    egit_object *wrapper;
    HASH_FIND_PTR(object_store, &wrapped, wrapper);
    egit_decref_repository_wrapper(wrapper);
}

void egit_decref_repository_wrapper(void *_wrapper)
{
    // The argument type must be void* to make this function work as an Emacs finalizer
    egit_object *wrapper = (egit_object*)_wrapper;

    // This function should never be called on something that doesn't wrap a repository
    assert(wrapper->type == EGIT_REPOSITORY);

    wrapper->refcount--;
    if (wrapper->refcount != 0)
        return;

    // First, delete the wrapper from the object store
    HASH_DEL(object_store, wrapper);

    // Free the wrapper and the wrappee
    git_repository_free(wrapper->ptr);
    free(wrapper);
}

/**
 * Increase the reference count of the given repository.
 * If the pointer does not exist in the object store, add it with a refcount of one.
 * Otherwise, increase the refcount by one.
 * @param obj The repository to store.
 * @return Pointer to the egit_object wrapper struct.
 */
static egit_object *egit_incref_repository(git_repository *wrapped)
{
    egit_object *wrapper;
    HASH_FIND_PTR(object_store, &wrapped, wrapper);

    if (wrapper)
        // Object is already stored, just incref
        wrapper->refcount++;

    else {
        // Object must be added
        wrapper = (egit_object*)malloc(sizeof(egit_object));
        wrapper->type = EGIT_REPOSITORY;
        wrapper->refcount = 1;
        wrapper->ptr = wrapped;
        HASH_ADD_PTR(object_store, ptr, wrapper);
    }

    return wrapper;
}

/**
 * Finalizer for user pointers with no children objects.
 * This frees the wrapper struct, its wrapped object and decreses the reference count on parent
 * objetcs, if any, potentially causing them to be freed.
 * This function is suitable to use as a finalizer for Emacs user pointers.
 */
static void egit_finalize(void* _obj)
{
    // The argument type must be void* to make this function work as an Emacs finalizer
    egit_object *obj = (egit_object*)_obj;

    // Decref any owner objects if applicable, and free the libgit2 struct.
    // Note that this object must be freed before potentially freeing owners.
    git_repository *repo = NULL;
    switch (obj->type) {
    case EGIT_COMMIT: case EGIT_TREE: case EGIT_BLOB: case EGIT_TAG: case EGIT_OBJECT:
        repo = git_object_owner(obj->ptr);
        git_object_free(obj->ptr);
        break;
    case EGIT_REFERENCE:
        repo = git_reference_owner(obj->ptr);
        git_reference_free(obj->ptr);
        break;
    default: break;
    }

    // Free the wrapper, then release the reference to the repository, if applicable
    free(obj);
    if (repo)
        egit_decref_repository(repo);
}

emacs_value egit_wrap_repository(emacs_env *env, git_repository *data)
{
    egit_object *wrapper = egit_incref_repository(data);
    return env->make_user_ptr(env, egit_decref_repository_wrapper, wrapper);
}

emacs_value egit_wrap(emacs_env *env, egit_type type, void* data)
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

    // Increase refcounts of owner object(s), if applicable
    switch (type) {
    case EGIT_COMMIT: case EGIT_TREE: case EGIT_BLOB: case EGIT_TAG: case EGIT_OBJECT:
        egit_incref_repository(git_object_owner(data)); break;
    case EGIT_REFERENCE:
        egit_incref_repository(git_reference_owner(data)); break;
    default: break;
    }

    // No need to worry about the refcount on these objects
    egit_object *wrapper = (egit_object*)malloc(sizeof(egit_object));
    wrapper->type = type;
    wrapper->ptr = data;

    return env->make_user_ptr(env, egit_finalize, wrapper);
}

typedef emacs_value (*func_1)(emacs_env*, emacs_value);
typedef emacs_value (*func_2)(emacs_env*, emacs_value, emacs_value);
typedef emacs_value (*func_3)(emacs_env*, emacs_value, emacs_value, emacs_value);
typedef emacs_value (*func_5)(emacs_env*, emacs_value, emacs_value, emacs_value,
                              emacs_value, emacs_value);
typedef emacs_value (*func_6)(emacs_env*, emacs_value, emacs_value, emacs_value,
                              emacs_value, emacs_value, emacs_value);

// Get an argument index, or nil. Useful for simulating optional arguments.
#define GET_SAFE(arglist, nargs, index) ((index) < (nargs) ? (arglist)[(index)] : em_nil)

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

emacs_value egit_dispatch_3(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_3 func = (func_3) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2));
}

emacs_value egit_dispatch_5(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_5 func = (func_5) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2),
                GET_SAFE(args, nargs, 3), GET_SAFE(args, nargs, 4));
}

emacs_value egit_dispatch_6(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_6 func = (func_6) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2),
                GET_SAFE(args, nargs, 3), GET_SAFE(args, nargs, 4), GET_SAFE(args, nargs, 5));
}

bool egit_dispatch_error(emacs_env *env, int retval)
{
    if (retval >= 0) return false;

    const git_error *err = giterr_last();
    if (!err) return false;

    em_signal_giterr(env, err->klass, err->message);
    return true;
}

EGIT_DOC(refcount, "OBJ", "Return the reference count of OBJ.");
static emacs_value egit_refcount(emacs_env *env, emacs_value val)
{
    if (egit_get_type(env, val) != EGIT_REPOSITORY)
        return em_nil;
    egit_object *wrapper = (egit_object*)env->get_user_ptr(env, val);
    return env->make_integer(env, wrapper->refcount);
}

EGIT_DOC(typeof, "OBJ", "Return the type of the git pointer OBJ, or nil.");
static emacs_value egit_typeof(emacs_env *env, emacs_value val)
{
    switch (egit_get_type(env, val)) {
    case EGIT_REPOSITORY: return em_repository;
    case EGIT_REFERENCE: return em_reference;
    case EGIT_COMMIT: return em_commit;
    case EGIT_TREE: return em_tree;
    case EGIT_BLOB: return em_blob;
    case EGIT_TAG: return em_tag;
    case EGIT_OBJECT: return em_object;
    default: return em_nil;
    }
}

EGIT_DOC(object_p, "OBJ", "Return non-nil if OBJ is a git object.");
static emacs_value egit_object_p(emacs_env *env, emacs_value obj)
{
    egit_type type = egit_get_type(env, obj);
    return (type == EGIT_COMMIT || type == EGIT_TREE || type == EGIT_BLOB ||
            type == EGIT_TAG || type == EGIT_OBJECT) ? em_t : em_nil;
}

EGIT_DOC(reference_p, "OBJ", "Return non-nil if OBJ is a git reference.");
static emacs_value egit_reference_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_REFERENCE ? em_t : em_nil;
}

EGIT_DOC(repository_p, "OBJ", "Return non-nil if OBJ is a git repository.");
static emacs_value egit_repository_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_REPOSITORY ? em_t : em_nil;
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
    // Type checkers
    DEFUN("libgit--refcount", refcount, 1, 1);
    DEFUN("libgit-typeof", typeof, 1, 1);
    DEFUN("libgit-object-p", object_p, 1, 1);
    DEFUN("libgit-reference-p", reference_p, 1, 1);
    DEFUN("libgit-repository-p", repository_p, 1, 1);

    // Clone
    DEFUN("libgit-clone", clone, 2, 2);

    // Object
    DEFUN("libgit-object-id", object_id, 1, 1);
    DEFUN("libgit-object-short-id", object_short_id, 1, 1);

    // Reference
    DEFUN("libgit-reference-create", reference_create, 3, 5);
    DEFUN("libgit-reference-create-matching", reference_create_matching, 3, 6);
    DEFUN("libgit-reference-dup", reference_dup, 1, 1);
    DEFUN("libgit-reference-dwim", reference_dwim, 2, 2);
    DEFUN("libgit-reference-lookup", reference_lookup, 2, 2);

    DEFUN("libgit-reference-list", reference_list, 1, 1);
    DEFUN("libgit-reference-name", reference_name, 1, 1);
    DEFUN("libgit-reference-owner", reference_owner, 1, 1);
    DEFUN("libgit-reference-peel", reference_peel, 1, 2);
    DEFUN("libgit-reference-resolve", reference_resolve, 1, 1);
    DEFUN("libgit-reference-shorthand", reference_shorthand, 1, 1);
    DEFUN("libgit-reference-symbolic-target", reference_symbolic_target, 1, 1);
    DEFUN("libgit-reference-target", reference_target, 1, 1);
    DEFUN("libgit-reference-target-peel", reference_target, 1, 1);
    DEFUN("libgit-reference-type", reference_type, 1, 1);

    DEFUN("libgit-reference-delete", reference_delete, 1, 1);
    DEFUN("libgit-reference-ensure-log", reference_ensure_log, 2, 2);
    DEFUN("libgit-reference-remove", reference_delete, 2, 2);

    DEFUN("libgit-reference-branch-p", reference_branch_p, 1, 1);
    DEFUN("libgit-reference-direct-p", reference_direct_p, 1, 1);
    DEFUN("libgit-reference-has-log-p", reference_has_log_p, 2, 2);
    DEFUN("libgit-reference-name-to-id", reference_name_to_id, 2, 2);
    DEFUN("libgit-reference-note-p", reference_note_p, 1, 1);
    DEFUN("libgit-reference-remote-p", reference_remote_p, 1, 1);
    DEFUN("libgit-reference-symbolic-p", reference_symbolic_p, 1, 1);
    DEFUN("libgit-reference-tag-p", reference_tag_p, 1, 1);
    DEFUN("libgit-reference-valid-name-p", reference_valid_name_p, 1, 1);

    // Repository
    DEFUN("libgit-repository-init", repository_init, 1, 2);
    DEFUN("libgit-repository-open", repository_open, 1, 1);
    DEFUN("libgit-repository-open-bare", repository_open_bare, 1, 1);

    DEFUN("libgit-repository-commondir", repository_commondir, 1, 1);
    DEFUN("libgit-repository-get-namespace", repository_get_namespace, 1, 1);
    DEFUN("libgit-repository-head", repository_head, 1, 1);
    DEFUN("libgit-repository-head-for-worktree", repository_head_for_worktree, 2, 2);
    DEFUN("libgit-repository-ident", repository_ident, 1, 1);
    DEFUN("libgit-repository-message", repository_message, 1, 1);
    DEFUN("libgit-repository-path", repository_path, 1, 1);
    DEFUN("libgit-repository-state", repository_state, 1, 1);
    DEFUN("libgit-repository-workdir", repository_workdir, 1, 1);

    DEFUN("libgit-repository-detach-head", repository_detach_head, 1, 1);
    DEFUN("libgit-repository-message-remove", repository_message_remove, 1, 1);
    DEFUN("libgit-repository-set-head", repository_set_head, 2, 2);
    DEFUN("libgit-repository-set-head-detached", repository_set_head_detached, 2, 2);
    DEFUN("libgit-repository-set-ident", repository_set_ident, 1, 3);
    DEFUN("libgit-repository-set-workdir", repository_set_workdir, 2, 3);
    DEFUN("libgit-repository-state-cleanup", repository_state_cleanup, 1, 1);

    DEFUN("libgit-repository-bare-p", repository_bare_p, 1, 1);
    DEFUN("libgit-repository-empty-p", repository_empty_p, 1, 1);
    DEFUN("libgit-repository-head-detached-p", repository_empty_p, 1, 1);
    DEFUN("libgit-repository-head-unborn-p", repository_empty_p, 1, 1);
    DEFUN("libgit-repository-shallow-p", repository_shallow_p, 1, 1);
    DEFUN("libgit-repository-worktree-p", repository_worktree_p, 1, 1);

    // Revparse
    DEFUN("libgit-revparse-single", revparse_single, 2, 2);
}
