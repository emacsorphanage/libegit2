#include <stdlib.h>
#include <assert.h>

#include "emacs-module.h"
#include "git2.h"
#include "uthash.h"

#include "interface.h"
#include "egit-blame.h"
#include "egit-blob.h"
#include "egit-branch.h"
#include "egit-checkout.h"
#include "egit-clone.h"
#include "egit-commit.h"
#include "egit-config.h"
#include "egit-describe.h"
#include "egit-diff.h"
#include "egit-ignore.h"
#include "egit-index.h"
#include "egit-libgit2.h"
#include "egit-object.h"
#include "egit-reference.h"
#include "egit-refspec.h"
#include "egit-remote.h"
#include "egit-repository.h"
#include "egit-revparse.h"
#include "egit-signature.h"
#include "egit-status.h"
#include "egit-submodule.h"
#include "egit-tag.h"
#include "egit-transaction.h"
#include "egit-tree.h"
#include "egit.h"

// Hash table of stored objects
egit_object *object_store = NULL;

egit_type egit_get_type(emacs_env *env, emacs_value _obj)
{
    if (!em_user_ptrp(env, _obj))
        return EGIT_UNKNOWN;
    egit_object *obj = (egit_object*) EM_EXTRACT_USER_PTR(_obj);
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

/**
 * Decrease the reference count of a wrapper object.
 * If the reference count reaches zero, the object will be freed,
 * @param data The wrapper object to decref.
 */
static void egit_decref_direct(egit_object *wrapper)
{
    wrapper->refcount--;
    if (wrapper->refcount != 0)
        return;

    // First, delete the wrapper from the object store
    HASH_DEL(object_store, wrapper);

    // Free the wrappee and the wrapper
    switch (wrapper->type) {
    case EGIT_BLAME:
        git_blame_free(wrapper->ptr);
        break;
    case EGIT_DIFF:
        git_diff_free(wrapper->ptr);
        break;
    case EGIT_INDEX:
        git_index_free(wrapper->ptr);
        break;
    case EGIT_REMOTE:
        git_remote_free(wrapper->ptr);
        break;
    case EGIT_REPOSITORY:
        git_repository_free(wrapper->ptr);
        break;
    default:
        // This function should never be called on something that isn't a reference-counted type
        assert(0);
    }
    free(wrapper);
}

/**
 * Increase the reference count of a reference-counted object.
 * If the pointer does not exist in the object store, add it with a refcount of one.
 * Otherwise, increase the refcount by one.
 * @param type The type of the object.
 * @param data The object to store.
 * @return Pointer to the egit_object wrapper struct.
 */
static egit_object *egit_incref(egit_type type, const void *data)
{
    egit_object *wrapper;
    HASH_FIND_PTR(object_store, &data, wrapper);

    if (wrapper)
        // Object is already stored, just incref
        wrapper->refcount++;

    else {
        // Object must be added
        wrapper = (egit_object*)malloc(sizeof(egit_object));
        wrapper->type = type;
        wrapper->refcount = 1;
        wrapper->ptr = (void*) data;
        HASH_ADD_PTR(object_store, ptr, wrapper);
    }

    return wrapper;
}

/**
 * Finalizer for user pointers.
 */
static void egit_finalize(void* _obj)
{
    // The argument type must be void* to make this function work as an Emacs finalizer
    egit_object *obj = (egit_object*)_obj;
    egit_object *parent = obj->parent;

    switch (obj->type) {
    // Reference counted types can be freed entirely from egit_decref
    case EGIT_BLAME:
    case EGIT_DIFF:
    case EGIT_INDEX:
    case EGIT_REMOTE:
    case EGIT_REPOSITORY:
        egit_decref_direct(obj);
        return;

    // Other types can be freed directly here
    case EGIT_COMMIT: case EGIT_TREE: case EGIT_BLOB: case EGIT_TAG: case EGIT_OBJECT:
        git_object_free(obj->ptr);
        break;

    case EGIT_REFERENCE:
        git_reference_free(obj->ptr);
        break;

    case EGIT_CONFIG:
        git_config_free(obj->ptr);
        break;

    case EGIT_SIGNATURE:
        git_signature_free(obj->ptr);
        break;

    case EGIT_TRANSACTION:
        git_transaction_free(obj->ptr);
        break;

    case EGIT_SUBMODULE:
        git_submodule_free(obj->ptr);
        break;

    default: break;
    }

    // Free the wrapper, then release the reference to the parent, if applicable
    free(obj);
    if (parent)
        egit_finalize(parent);
}

emacs_value egit_wrap(emacs_env *env, egit_type type, const void* data, egit_object *parent)
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
    if (parent)
        parent->refcount++;
    else {
        switch (type) {
        case EGIT_COMMIT: case EGIT_TREE: case EGIT_BLOB: case EGIT_TAG: case EGIT_OBJECT:
            parent = egit_incref(EGIT_REPOSITORY, git_object_owner(data));
            break;
        case EGIT_INDEX:
            parent = egit_incref(EGIT_REPOSITORY, git_index_owner(data));
            break;
        case EGIT_REFERENCE:
            parent = egit_incref(EGIT_REPOSITORY, git_reference_owner(data));
            break;
        case EGIT_REMOTE:
            parent = egit_incref(EGIT_REPOSITORY, git_remote_owner(data));
            break;
        case EGIT_SUBMODULE:
            // Why isn't git_submodule_owner const?
            parent = egit_incref(EGIT_REPOSITORY, git_submodule_owner((void*) data));
            break;
        default: break;
        }
    }

    egit_object *wrapper;
    switch (type) {
    case EGIT_BLAME:
    case EGIT_DIFF:
    case EGIT_INDEX:
    case EGIT_REMOTE:
    case EGIT_REPOSITORY:
        wrapper = egit_incref(type, data);
        break;
    default:
        wrapper = (egit_object*) malloc(sizeof(egit_object));
        wrapper->type = type;
        wrapper->ptr = (void*) data;
        break;
    }
    wrapper->parent = parent;

    return env->make_user_ptr(env, egit_finalize, wrapper);
}

typedef emacs_value (*func_0)(emacs_env*);
typedef emacs_value (*func_1)(emacs_env*, emacs_value);
typedef emacs_value (*func_2)(emacs_env*, emacs_value, emacs_value);
typedef emacs_value (*func_3)(emacs_env*, emacs_value, emacs_value, emacs_value);
typedef emacs_value (*func_4)(emacs_env*, emacs_value, emacs_value, emacs_value,
                              emacs_value);
typedef emacs_value (*func_5)(emacs_env*, emacs_value, emacs_value, emacs_value,
                              emacs_value, emacs_value);
typedef emacs_value (*func_6)(emacs_env*, emacs_value, emacs_value, emacs_value,
                              emacs_value, emacs_value, emacs_value);

// Get an argument index, or nil. Useful for simulating optional arguments.
#define GET_SAFE(arglist, nargs, index) ((index) < (nargs) ? (arglist)[(index)] : em_nil)

static emacs_value egit_dispatch_0(emacs_env *env, __attribute__((unused)) ptrdiff_t nargs,
                            __attribute__((unused)) emacs_value *args, void *data)
{
    func_0 func = (func_0) data;
    return func(env);
}

static emacs_value egit_dispatch_1(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_1 func = (func_1) data;
    return func(env, GET_SAFE(args, nargs, 0));
}

static emacs_value egit_dispatch_2(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_2 func = (func_2) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1));
}

static emacs_value egit_dispatch_3(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_3 func = (func_3) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2));
}

static emacs_value egit_dispatch_4(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_4 func = (func_4) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2),
                GET_SAFE(args, nargs, 3));
}

static emacs_value egit_dispatch_5(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_5 func = (func_5) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2),
                GET_SAFE(args, nargs, 3), GET_SAFE(args, nargs, 4));
}

static emacs_value egit_dispatch_6(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
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

    emacs_value error;
    switch (err->klass) {
    case GITERR_NOMEMORY: error = em_giterr_nomemory; break;
    case GITERR_OS: error = em_giterr_os; break;
    case GITERR_INVALID: error = em_giterr_invalid; break;
    case GITERR_REFERENCE: error = em_giterr_reference; break;
    case GITERR_ZLIB: error = em_giterr_zlib; break;
    case GITERR_REPOSITORY: error = em_giterr_repository; break;
    case GITERR_CONFIG: error = em_giterr_config; break;
    case GITERR_REGEX: error = em_giterr_regex; break;
    case GITERR_ODB: error = em_giterr_odb; break;
    case GITERR_INDEX: error = em_giterr_index; break;
    case GITERR_OBJECT: error = em_giterr_object; break;
    case GITERR_NET: error = em_giterr_net; break;
    case GITERR_TAG: error = em_giterr_tag; break;
    case GITERR_TREE: error = em_giterr_tree; break;
    case GITERR_INDEXER: error = em_giterr_indexer; break;
    case GITERR_SSL: error = em_giterr_ssl; break;
    case GITERR_SUBMODULE: error = em_giterr_submodule; break;
    case GITERR_THREAD: error = em_giterr_thread; break;
    case GITERR_STASH: error = em_giterr_stash; break;
    case GITERR_CHECKOUT: error = em_giterr_checkout; break;
    case GITERR_FETCHHEAD: error = em_giterr_fetchhead; break;
    case GITERR_MERGE: error = em_giterr_merge; break;
    case GITERR_SSH: error = em_giterr_ssh; break;
    case GITERR_FILTER: error = em_giterr_filter; break;
    case GITERR_REVERT: error = em_giterr_revert; break;
    case GITERR_CALLBACK: error = em_giterr_callback; break;
    case GITERR_CHERRYPICK: error = em_giterr_cherrypick; break;
    case GITERR_DESCRIBE: error = em_giterr_describe; break;
    case GITERR_REBASE: error = em_giterr_rebase; break;
    case GITERR_FILESYSTEM: error = em_giterr_filesystem; break;
    case GITERR_PATCH: error = em_giterr_patch; break;
    case GITERR_WORKTREE: error = em_giterr_worktree; break;
    case GITERR_SHA1: error = em_giterr_sha1; break;
    default: error = em_giterr; break;
    }

    em_signal(env, error, err->message);
    return true;
}

EGIT_DOC(refcount, "OBJ", "Return the reference count of OBJ.");
static emacs_value egit_refcount(emacs_env *env, emacs_value val)
{
    if (egit_get_type(env, val) != EGIT_REPOSITORY)
        return em_nil;
    egit_object *wrapper = (egit_object*) EM_EXTRACT_USER_PTR(val);
    return EM_INTEGER(wrapper->refcount);
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
    case EGIT_BLAME: return em_blame;
    case EGIT_BLAME_HUNK: return em_blame_hunk;
    case EGIT_CONFIG: return em_config;
    case EGIT_TRANSACTION: return em_transaction;
    case EGIT_INDEX: return em_index;
    case EGIT_INDEX_ENTRY: return em_index_entry;
    case EGIT_DIFF: return em_diff;
    case EGIT_DIFF_DELTA: return em_diff_delta;
    case EGIT_DIFF_BINARY: return em_diff_binary;
    case EGIT_DIFF_HUNK: return em_diff_hunk;
    case EGIT_DIFF_LINE: return em_diff_line;
    case EGIT_REMOTE: return em_remote;
    case EGIT_REFSPEC: return em_refspec;
    case EGIT_SUBMODULE: return em_submodule;
    default: return em_nil;
    }
}

EGIT_DOC(blame_p, "OBJ", "Return non-nil if OBJ is a git blame.");
static emacs_value egit_blame_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_BLAME ? em_t : em_nil;
}

EGIT_DOC(blame_hunk_p, "OBJ", "Return non-nil if OBJ is a git blame hunk.");
static emacs_value egit_blame_hunk_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_BLAME_HUNK ? em_t : em_nil;
}

EGIT_DOC(blob_p, "OBJ", "Return non-nil if OBJ is a git blob.");
static emacs_value egit_blob_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_BLOB ? em_t : em_nil;
}

EGIT_DOC(commit_p, "OBJ", "Return non-nil if OBJ is a git commit.");
static emacs_value egit_commit_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_COMMIT ? em_t : em_nil;
}

EGIT_DOC(config_p, "OBJ", "Return non-nil if OBJ is a git config.");
static emacs_value egit_config_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_CONFIG ? em_t : em_nil;
}

EGIT_DOC(diff_p, "OBJ", "Return non-nil if OBJ is a git diff.");
static emacs_value egit_diff_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_DIFF ? em_t : em_nil;
}

EGIT_DOC(diff_delta_p, "OBJ", "Return non-nil if OBJ is a git diff delta.");
static emacs_value egit_diff_delta_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_DIFF_DELTA ? em_t : em_nil;
}

EGIT_DOC(diff_binary_p, "OBJ", "Return non-nil if OBJ is a git diff binary.");
static emacs_value egit_diff_binary_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_DIFF_BINARY ? em_t : em_nil;
}

EGIT_DOC(diff_hunk_p, "OBJ", "Return non-nil if OBJ is a git diff hunk.");
static emacs_value egit_diff_hunk_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_DIFF_HUNK ? em_t : em_nil;
}

EGIT_DOC(diff_line_p, "OBJ", "Return non-nil if OBJ is a git diff line.");
static emacs_value egit_diff_line_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_DIFF_LINE ? em_t : em_nil;
}

EGIT_DOC(index_p, "OBJ", "Return non-nil if OBJ is a git index.");
static emacs_value egit_index_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_INDEX ? em_t : em_nil;
}

EGIT_DOC(index_entry_p, "OBJ", "Return non-nil if OBJ is a git index entry.");
static emacs_value egit_index_entry_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_INDEX_ENTRY ? em_t : em_nil;
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

EGIT_DOC(refspec_p, "OBJ", "Return non-nil if OBJ is a git refspec.");
static emacs_value egit_refspec_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_REFSPEC ? em_t : em_nil;
}

EGIT_DOC(remote_p, "OBJ", "Return non-nil if OBJ is a git remote.");
static emacs_value egit_remote_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_REMOTE ? em_t : em_nil;
}

EGIT_DOC(repository_p, "OBJ", "Return non-nil if OBJ is a git repository.");
static emacs_value egit_repository_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_REPOSITORY ? em_t : em_nil;
}

EGIT_DOC(signature_p, "OBJ", "Return non-nil if OBJ is a git signature.");
static emacs_value egit_signature_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_SIGNATURE ? em_t : em_nil;
}

EGIT_DOC(submodule_p, "OBJ", "Return non-nil if OBJ is a git submodule.");
static emacs_value egit_submodule_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_SUBMODULE ? em_t : em_nil;
}

EGIT_DOC(tag_p, "OBJ", "Return non-nil if OBJ is a git tag.");
static emacs_value egit_tag_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_TAG ? em_t : em_nil;
}

EGIT_DOC(transaction_p, "OBJ", "Return non-nil if OBJ is a git transaction.");
static emacs_value egit_transaction_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_TRANSACTION ? em_t : em_nil;
}

EGIT_DOC(tree_p, "OBJ", "Return non-nil if OBJ is a git tree.");
static emacs_value egit_tree_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_TREE ? em_t : em_nil;
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
    DEFUN("libgit-blame-p", blame_p, 1, 1);
    DEFUN("libgit-blame-hunk-p", blame_hunk_p, 1, 1);
    DEFUN("libgit-blob-p", blob_p, 1, 1);
    DEFUN("libgit-commit-p", commit_p, 1, 1);
    DEFUN("libgit-config-p", config_p, 1, 1);
    DEFUN("libgit-diff-p", diff_p, 1, 1);
    DEFUN("libgit-diff-delta-p", diff_delta_p, 1, 1);
    DEFUN("libgit-diff-binary-p", diff_binary_p, 1, 1);
    DEFUN("libgit-diff-hunk-p", diff_hunk_p, 1, 1);
    DEFUN("libgit-diff-line-p", diff_line_p, 1, 1);
    DEFUN("libgit-index-p", index_p, 1, 1);
    DEFUN("libgit-index-entry-p", index_entry_p, 1, 1);
    DEFUN("libgit-object-p", object_p, 1, 1);
    DEFUN("libgit-reference-p", reference_p, 1, 1);
    DEFUN("libgit-refspec-p", refspec_p, 1, 1);
    DEFUN("libgit-remote-p", remote_p, 1, 1);
    DEFUN("libgit-repository-p", repository_p, 1, 1);
    DEFUN("libgit-signature-p", signature_p, 1, 1);
    DEFUN("libgit-submodule-p", submodule_p, 1, 1);
    DEFUN("libgit-tag-p", tag_p, 1, 1);
    DEFUN("libgit-transaction-p", transaction_p, 1, 1);
    DEFUN("libgit-tree-p", tree_p, 1, 1);

    // Libgit2 (not namespaced as others!)
    DEFUN("libgit-feature-p", libgit2_feature_p, 1, 1);
    DEFUN("libgit-version", libgit2_version, 0, 0);

    // Blame
    DEFUN("libgit-blame-file", blame_file, 2, 3);
    DEFUN("libgit-blame-get-hunk-byindex", blame_get_hunk_byindex, 2, 2);
    DEFUN("libgit-blame-get-hunk-byline", blame_get_hunk_byline, 2, 2);
    DEFUN("libgit-blame-get-hunk-count", blame_get_hunk_count, 1, 1);

    DEFUN("libgit-blame-hunk-commit-id", blame_hunk_commit_id, 1, 2);
    DEFUN("libgit-blame-hunk-lines", blame_hunk_lines, 1, 1);
    DEFUN("libgit-blame-hunk-orig-path", blame_hunk_orig_path, 1, 1);
    DEFUN("libgit-blame-hunk-signature", blame_hunk_signature, 1, 2);
    DEFUN("libgit-blame-hunk-start-line-number", blame_hunk_start_line_number, 1, 2);

    // Blob
    DEFUN("libgit-blob-lookup", blob_lookup, 2, 2);
    DEFUN("libgit-blob-lookup-prefix", blob_lookup_prefix, 2, 2);

    DEFUN("libgit-blob-binary-p", blob_binary_p, 1, 1);
    DEFUN("libgit-blob-filtered-content", blob_filtered_content, 2, 3);
    DEFUN("libgit-blob-id", blob_id, 1, 1);
    DEFUN("libgit-blob-owner", blob_owner, 1, 1);
    DEFUN("libgit-blob-rawcontent", blob_rawcontent, 1, 1);
    DEFUN("libgit-blob-rawsize", blob_rawsize, 1, 1);

    // Branch
    DEFUN("libgit-branch-create", branch_create, 3, 4);
    DEFUN("libgit-branch-create-from-annotated", branch_create_from_annotated, 3, 4);
    DEFUN("libgit-branch-lookup", branch_lookup, 2, 3);
    DEFUN("libgit-branch-delete", branch_delete, 1, 1);
    DEFUN("libgit-branch-checked-out-p", branch_checked_out_p, 1, 1);
    DEFUN("libgit-branch-head-p", branch_head_p, 1, 1);
    DEFUN("libgit-branch-name", branch_name, 1, 1);
    DEFUN("libgit-branch-remote-name", branch_remote_name, 2, 2);
    DEFUN("libgit-branch-upstream", branch_upstream, 1, 1);
    DEFUN("libgit-branch-upstream-name", branch_upstream_name, 2, 2);
    DEFUN("libgit-branch-upstream-remote", branch_upstream_remote, 2, 2);

    // Checkout
    DEFUN("libgit-checkout-head", checkout_head, 1, 2);
    DEFUN("libgit-checkout-index", checkout_index, 1, 3);
    DEFUN("libgit-checkout-tree", checkout_tree, 1, 3);

    // Clone
    DEFUN("libgit-clone", clone, 2, 2);

    // Commit
    DEFUN("libgit-commit-lookup", commit_lookup, 2, 2);
    DEFUN("libgit-commit-lookup-prefix", commit_lookup_prefix, 2, 2);

    DEFUN("libgit-commit-author", commit_author, 1, 1);
    DEFUN("libgit-commit-body", commit_body, 1, 1);
    DEFUN("libgit-commit-committer", commit_committer, 1, 1);
    DEFUN("libgit-commit-id", commit_id, 1, 1);
    DEFUN("libgit-commit-message", commit_message, 1, 1);
    DEFUN("libgit-commit-nth-gen-ancestor", commit_nth_gen_ancestor, 2, 2);
    DEFUN("libgit-commit-owner", commit_owner, 1, 1);
    DEFUN("libgit-commit-parent", commit_parent, 1, 2);
    DEFUN("libgit-commit-parent-id", commit_parent_id, 1, 2);
    DEFUN("libgit-commit-parentcount", commit_parentcount, 1, 1);
    DEFUN("libgit-commit-summary", commit_summary, 1, 1);
    DEFUN("libgit-commit-time", commit_time, 1, 1);
    DEFUN("libgit-commit-tree", commit_tree, 1, 1);
    DEFUN("libgit-commit-tree-id", commit_tree_id, 1, 1);

    // Config
    DEFUN("libgit-config-snapshot", config_snapshot, 1, 1);

    DEFUN("libgit-config-get-bool", config_get_bool, 2, 2);
    DEFUN("libgit-config-get-int", config_get_int, 2, 2);
    DEFUN("libgit-config-get-path", config_get_path, 2, 2);
    DEFUN("libgit-config-get-string", config_get_string, 2, 2);
    DEFUN("libgit-config-lock", config_lock, 1, 1);

    DEFUN("libgit-config-set-bool", config_set_bool, 3, 3);
    DEFUN("libgit-config-set-int", config_set_int, 3, 3);
    DEFUN("libgit-config-set-string", config_set_string, 3, 3);

    DEFUN("libgit-config-find-global", config_find_global, 0, 0);
    DEFUN("libgit-config-find-programdata", config_find_programdata, 0, 0);
    DEFUN("libgit-config-find-system", config_find_system, 0, 0);
    DEFUN("libgit-config-find-xdg", config_find_xdg, 0, 0);

    // Describe
    DEFUN("libgit-describe-commit", describe_commit, 1, 2);
    DEFUN("libgit-describe-workdir", describe_workdir, 1, 2);

    // Diff
    DEFUN("libgit-diff-index-to-index", diff_index_to_index, 3, 4);
    DEFUN("libgit-diff-index-to-workdir", diff_index_to_workdir, 1, 3);
    DEFUN("libgit-diff-tree-to-index", diff_tree_to_index, 1, 4);
    DEFUN("libgit-diff-tree-to-tree", diff_tree_to_tree, 1, 4);
    DEFUN("libgit-diff-tree-to-workdir", diff_tree_to_workdir, 1, 3);
    DEFUN("libgit-diff-tree-to-workdir-with-index", diff_tree_to_workdir_with_index, 1, 3);

    DEFUN("libgit-diff-foreach", diff_foreach, 2, 5);
    DEFUN("libgit-diff-print", diff_print, 1, 3);

    DEFUN("libgit-diff-delta-file-id", diff_delta_file_id, 2, 2);
    DEFUN("libgit-diff-delta-file-path", diff_delta_file_path, 2, 2);
    DEFUN("libgit-diff-delta-nfiles", diff_delta_nfiles, 1, 1);
    DEFUN("libgit-diff-delta-similarity", diff_delta_similarity, 1, 1);
    DEFUN("libgit-diff-delta-status", diff_delta_status, 1, 1);
    DEFUN("libgit-diff-delta-file-exists-p", diff_delta_file_exists_p, 2, 2);

    DEFUN("libgit-diff-hunk-header", diff_hunk_header, 1, 1);
    DEFUN("libgit-diff-hunk-lines", diff_hunk_lines, 2, 2);
    DEFUN("libgit-diff-hunk-start", diff_hunk_start, 2, 2);

    DEFUN("libgit-diff-line-origin", diff_line_origin, 1, 1);
    DEFUN("libgit-diff-line-lineno", diff_line_lineno, 2, 2);
    DEFUN("libgit-diff-line-content", diff_line_content, 1, 1);

    DEFUN("libgit-diff-get-delta", diff_get_delta, 2, 2);
    DEFUN("libgit-diff-num-deltas", diff_num_deltas, 1, 2);

    // Ignore
    DEFUN("libgit-ignore-add-rule", add_rule, 2, 2);
    DEFUN("libgit-ignore-clear-internal-rules", clear_internal_rules, 1, 1);
    DEFUN("libgit-ignore-path-ignored-p", path_ignored_p, 2, 2);

    // Index
    DEFUN("libgit-index-caps", index_caps, 1, 1);
    DEFUN("libgit-index-checksum", index_checksum, 1, 1);
    DEFUN("libgit-index-conflict-foreach", index_conflict_foreach, 2, 2);
    DEFUN("libgit-index-conflict-get", index_conflict_get, 2, 2);
    DEFUN("libgit-index-entry-id", index_entry_id, 1, 1);
    DEFUN("libgit-index-entry-path", index_entry_path, 1, 1);
    DEFUN("libgit-index-entry-stage", index_entry_stage, 1, 1);
    DEFUN("libgit-index-entrycount", index_entrycount, 1, 1);
    DEFUN("libgit-index-get-byindex", index_get_byindex, 2, 2);
    DEFUN("libgit-index-get-bypath", index_get_bypath, 2, 3);
    DEFUN("libgit-index-owner", index_owner, 1, 1);
    DEFUN("libgit-index-path", index_path, 1, 1);
    DEFUN("libgit-index-version", index_version, 1, 1);
    DEFUN("libgit-index-conflicts-p", index_conflicts_p, 1, 1);

    DEFUN("libgit-index-add-all", index_add_all, 1, 4);
    DEFUN("libgit-index-add-bypath", index_add_bypath, 2, 2);
    DEFUN("libgit-index-clear", index_clear, 1, 1);
    DEFUN("libgit-index-read", index_read, 1, 2);
    DEFUN("libgit-index-write", index_write, 1, 1);

    // Object
    DEFUN("libgit-object-lookup", object_lookup, 2, 3);
    DEFUN("libgit-object-lookup-prefix", object_id, 2, 3);

    DEFUN("libgit-object-id", object_id, 1, 1);
    DEFUN("libgit-object-owner", object_owner, 1, 1);
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

    // Refspec
    DEFUN("libgit-refspec-direction", refspec_direction, 1, 1);
    DEFUN("libgit-refspec-dst", refspec_dst, 1, 1);
    DEFUN("libgit-refspec-src", refspec_src, 1, 1);
    DEFUN("libgit-refspec-string", refspec_string, 1, 1);
    DEFUN("libgit-refspec-dst-matches-p", refspec_dst_matches_p, 2, 2);
    DEFUN("libgit-refspec-force-p", refspec_force_p, 1, 1);
    DEFUN("libgit-refspec-src-matches-p", refspec_src_matches_p, 2, 2);

    // Remote
    DEFUN("libgit-remote-lookup", remote_lookup, 2, 2);
    DEFUN("libgit-remote-autotag", remote_autotag, 1, 1);
    DEFUN("libgit-remote-get-refspec", remote_get_refspec, 2, 2);
    DEFUN("libgit-remote-get-refspecs", remote_get_refspecs, 2, 2);
    DEFUN("libgit-remote-name", remote_name, 1, 1);
    DEFUN("libgit-remote-owner", remote_owner, 1, 1);
    DEFUN("libgit-remote-pushurl", remote_pushurl, 1, 1);
    DEFUN("libgit-remote-refspec-count", remote_refspec_count, 1, 1);
    DEFUN("libgit-remote-url", remote_url, 1, 1);
    DEFUN("libgit-remote-list", remote_list, 1, 1);
    DEFUN("libgit-remote-valid-name-p", remote_valid_name_p, 1, 1);

    DEFUN("libgit-remote-fetch", remote_fetch, 1, 4);
    DEFUN("libgit-remote-push", remote_push, 1, 3);

    // Repository
    DEFUN("libgit-repository-init", repository_init, 1, 2);
    DEFUN("libgit-repository-open", repository_open, 1, 1);
    DEFUN("libgit-repository-open-bare", repository_open_bare, 1, 1);

    DEFUN("libgit-repository-commondir", repository_commondir, 1, 1);
    DEFUN("libgit-repository-config", repository_config, 1, 1);
    DEFUN("libgit-repository-get-namespace", repository_get_namespace, 1, 1);
    DEFUN("libgit-repository-head", repository_head, 1, 1);
    DEFUN("libgit-repository-head-for-worktree", repository_head_for_worktree, 2, 2);
    DEFUN("libgit-repository-ident", repository_ident, 1, 1);
    DEFUN("libgit-repository-index", repository_index, 1, 1);
    DEFUN("libgit-repository-message", repository_message, 1, 1);
    DEFUN("libgit-repository-path", repository_path, 1, 1);
    DEFUN("libgit-repository-state", repository_state, 1, 1);
    DEFUN("libgit-repository-workdir", repository_workdir, 1, 1);

    DEFUN("libgit-repository-detach-head", repository_detach_head, 1, 1);
    DEFUN("libgit-repository-message-remove", repository_message_remove, 1, 1);
    DEFUN("libgit-repository-set-head", repository_set_head, 2, 2);
    DEFUN("libgit-repository-set-head-detached", repository_set_head_detached, 2, 2);
    DEFUN("libgit-repository-set-ident", repository_set_ident, 1, 3);
    DEFUN("libgit-repository-set-namespace", repository_set_namespace, 2, 2);
    DEFUN("libgit-repository-set-workdir", repository_set_workdir, 2, 3);
    DEFUN("libgit-repository-state-cleanup", repository_state_cleanup, 1, 1);

    DEFUN("libgit-repository-bare-p", repository_bare_p, 1, 1);
    DEFUN("libgit-repository-empty-p", repository_empty_p, 1, 1);
    DEFUN("libgit-repository-head-detached-p", repository_empty_p, 1, 1);
    DEFUN("libgit-repository-head-unborn-p", repository_empty_p, 1, 1);
    DEFUN("libgit-repository-shallow-p", repository_shallow_p, 1, 1);
    DEFUN("libgit-repository-worktree-p", repository_worktree_p, 1, 1);

    DEFUN("libgit-repository-discover", repository_discover, 0, 3);

    // Revparse
    DEFUN("libgit-revparse-single", revparse_single, 2, 2);

    // Signature
    DEFUN("libgit-signature-default", signature_default, 1, 1);
    DEFUN("libgit-signature-name", signature_name, 1, 1);
    DEFUN("libgit-signature-email", signature_email, 1, 1);
    DEFUN("libgit-signature-time", signature_time, 1, 1);

    // Status
    DEFUN("libgit-status-decode", status_decode, 1, 1);
    DEFUN("libgit-status-file", status_file, 2, 2);
    DEFUN("libgit-status-should-ignore-p", status_should_ignore_p, 2, 2);
    DEFUN("libgit-status-foreach", status_foreach, 2, 6);

    // Submodule
    DEFUN("libgit-submodule-lookup", submodule_lookup, 2, 2);
    DEFUN("libgit-submodule-foreach", submodule_foreach, 2, 2);

    DEFUN("libgit-submodule-branch", submodule_branch, 1, 1);
    DEFUN("libgit-submodule-head-id", submodule_head_id, 1, 1);
    DEFUN("libgit-submodule-ignore", submodule_ignore, 1, 1);
    DEFUN("libgit-submodule-index-id", submodule_index_id, 1, 1);
    DEFUN("libgit-submodule-location", submodule_location, 1, 2);
    DEFUN("libgit-submodule-name", submodule_name, 1, 1);
    DEFUN("libgit-submodule-open", submodule_open, 1, 1);
    DEFUN("libgit-submodule-owner", submodule_owner, 1, 1);
    DEFUN("libgit-submodule-path", submodule_path, 1, 1);
    DEFUN("libgit-submodule-status", submodule_status, 2, 4);
    DEFUN("libgit-submodule-url", submodule_url, 1, 1);
    DEFUN("libgit-submodule-wd-id", submodule_wd_id, 1, 1);

    DEFUN("libgit-submodule-reload", submodule_reload, 1, 2);

    // Tag
    DEFUN("libgit-tag-lookup", tag_lookup, 2, 2);
    DEFUN("libgit-tag-lookup-prefix", tag_lookup_prefix, 2, 2);
    DEFUN("libgit-tag-foreach", tag_foreach, 2, 2);
    DEFUN("libgit-tag-id", tag_id, 1, 1);
    DEFUN("libgit-tag-owner", tag_owner, 1, 1);
    DEFUN("libgit-tag-message", tag_message, 1, 1);
    DEFUN("libgit-tag-name", tag_name, 1, 1);
    DEFUN("libgit-tag-peel", tag_peel, 1, 1);
    DEFUN("libgit-tag-target", tag_target, 1, 1);
    DEFUN("libgit-tag-target-id", tag_target_id, 1, 1);
    DEFUN("libgit-tag-target-type", tag_target_type, 1, 1);
    DEFUN("libgit-tag-list", tag_list, 1, 2);

    // Transaction
    DEFUN("libgit-transaction-commit", transaction_commit, 1, 1);

    // Tree
    DEFUN("libgit-tree-lookup", tree_lookup, 2, 2);
    DEFUN("libgit-tree-lookup-prefix", tree_lookup_prefix, 2, 2);

    DEFUN("libgit-tree-id", tree_id, 1, 1);
    DEFUN("libgit-tree-owner", tree_owner, 1, 1);

    DEFUN("libgit-tree-entry-byid", tree_entry_byid, 2, 2);
    DEFUN("libgit-tree-entry-byindex", tree_entry_byindex, 2, 2);
    DEFUN("libgit-tree-entry-byname", tree_entry_byname, 2, 2);
    DEFUN("libgit-tree-entry-bypath", tree_entry_bypath, 2, 2);
    DEFUN("libgit-tree-entrycount", tree_entrycount, 1, 1);
    DEFUN("libgit-tree-id", tree_id, 1, 1);
    DEFUN("libgit-tree-owner", tree_owner, 1, 1);

    DEFUN("libgit-tree-walk", tree_walk, 3, 3);
}
