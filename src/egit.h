#include "emacs-module.h"
#include "git2.h"
#include "uthash.h"
#include "git2.h"

#ifndef EGIT_H
#define EGIT_H

/**
 * Macro that defines a docstring for a function.
 * @param name The function name (without egit_ prefix).
 * @param args The argument list as visible from Emacs (without parens).
 * @param docstring The rest of the documentation.
 */
#define EGIT_DOC(name, args, docstring)                                 \
    const char *egit_##name##__doc = (docstring "\n\n(fn " args ")")

/**
 * Macro that declares a function and its docstring variable.
 * @param name The function name (without egit_ prefix)
 * @param ... The function arguments (without emacs_env)
 */
#define EGIT_DEFUN(name, ...)                                   \
    extern const char *egit_##name##__doc;                      \
    emacs_value egit_##name(emacs_env *env, __VA_ARGS__)

// Variant of EGIT_DEFUN for zero parameters.
#define EGIT_DEFUN_0(name)                      \
    extern const char *egit_##name##__doc;      \
    emacs_value egit_##name(emacs_env *env)

// Assert that VAL is a git blame, signal an error and return otherwise.
#define EGIT_ASSERT_BLAME(val)                                          \
    do { if (!egit_assert_type(env, (val), EGIT_BLAME, em_libgit_blame_p)) return em_nil; } while (0)

// Assert that VAL is a git commit, signal an error and return otherwise.
#define EGIT_ASSERT_COMMIT(val)                                         \
    do { if (!egit_assert_type(env, (val), EGIT_COMMIT, em_libgit_commit_p)) return em_nil; } while (0)

// Assert that VAL is a git config, signal an error and return otherwise.
#define EGIT_ASSERT_CONFIG(val)                                         \
    do { if (!egit_assert_type(env, (val), EGIT_CONFIG, em_libgit_config_p)) return em_nil; } while (0)

// Assert that VAL is a git index, signal an error and return otherwise.
#define EGIT_ASSERT_INDEX(val)                                          \
    do { if (!egit_assert_type(env, (val), EGIT_INDEX, em_libgit_index_p)) return em_nil; } while (0)

// Assert that VAL is a git index entry, signal an error and return otherwise.
#define EGIT_ASSERT_INDEX_ENTRY(val)                                    \
    do { if (!egit_assert_type(env, (val), EGIT_INDEX_ENTRY, em_libgit_index_entry_p)) return em_nil; } while (0)

// Assert that VAL is a git object, signal an error and return otherwise.
#define EGIT_ASSERT_OBJECT(val)                                         \
    do { if (!egit_assert_object(env, (val))) return em_nil; } while (0)

// Assert that VAL is a git reference, signal an error and return otherwise.
#define EGIT_ASSERT_REFERENCE(val)                                      \
    do { if (!egit_assert_type(env, (val), EGIT_REFERENCE, em_libgit_reference_p)) return em_nil; } while (0)

// Assert that VAL is a git repository, signal an error and return otherwise.
#define EGIT_ASSERT_REPOSITORY(val)                                     \
    do { if (!egit_assert_type(env, (val), EGIT_REPOSITORY, em_libgit_repository_p)) return em_nil; } while (0)

// Assert that VAL is a signature, signal an error and return otherwise.
#define EGIT_ASSERT_SIGNATURE(val)                                     \
    do { if (!egit_assert_type(env, (val), EGIT_SIGNATURE, em_libgit_signature_p)) return em_nil; } while (0)

// Assert that VAL is a transaction, signal an error and return otherwise.
#define EGIT_ASSERT_TRANSACTION(val)                                    \
    do { if (!egit_assert_type(env, (val), EGIT_TRANSACTION, em_libgit_transaction_p)) return em_nil; } while (0)

// Assert that VAL is a transaction, signal an error and return otherwise.
#define EGIT_ASSERT_TREE(val)                                           \
    do { if (!egit_assert_type(env, (val), EGIT_TREE, em_libgit_tree_p)) return em_nil; } while (0)

// Assert that VAL is a signature or nil, signal an error and return otherwise.
#define EGIT_ASSERT_SIGNATURE_OR_NIL(val) \
    do { if (EM_EXTRACT_BOOLEAN(val)) EGIT_ASSERT_SIGNATURE(val); } while (0)

/**
 * Extract a libgit git_??? struct from an emacs_value.
 * Caller is responsible for ensuring that this is a valid operation.
 */
#define EGIT_EXTRACT(val) (((egit_object*) EM_EXTRACT_USER_PTR(val))->ptr)

/**
 * Extract a libgit git_??? struct from an emacs_value, or NULL.
 * Caller is responsible for ensuring that this is a valid operation.
 */
#define EGIT_EXTRACT_OR_NULL(val)                                \
  (EM_EXTRACT_BOOLEAN(val) ? EGIT_EXTRACT(val) : NULL);

/**
 * Extract a git_oid from an emacs_value.
 * Caller is responsible for ensuring that the emacs_value is a string.
 */
#define EGIT_EXTRACT_OID(val, tgt)                      \
    do {                                                \
        char *__str = em_get_string(env, (val));        \
        int __retval = git_oid_fromstrp(&(tgt), __str); \
        free(__str);                                    \
        EGIT_CHECK_ERROR(__retval);                     \
    } while (0)

/**
 * Extract a partial git_oid from an emacs_value and store its length.
 * Caller is responsible for ensuring that the emacs_value is a string.
 */
#define EGIT_EXTRACT_OID_PREFIX(val, tgt, tgt_len)      \
    do {                                                \
        char *__str = em_get_string(env, (val));        \
        tgt_len = strlen(__str);                        \
        int __retval = git_oid_fromstrp(&(tgt), __str); \
        free(__str);                                    \
        EGIT_CHECK_ERROR(__retval);                     \
    } while (0)

/**
 * If libgit2 signalled an error, pass the error on to Emacs and return.
 * @param val A libgit2 return value (negative value indicates error).
 */
#define EGIT_CHECK_ERROR(val)                                           \
    do { if (egit_dispatch_error(env, (val))) return em_nil; } while (0)

/**
 * Convert a git_buf to an Emacs string and return it, freeing the git_buf in the process.
 */
#define EGIT_RET_BUF_AS_STRING(buf)                                     \
    do {                                                                \
        emacs_value ret = env->make_string(env, (buf).ptr, (buf).size); \
        git_buf_free(&(buf));                                           \
        return ret;                                                     \
    } while (0)

/**
 * Enum used to distinguish between various types of git_??? structs.
 */
typedef enum {
    EGIT_UNKNOWN,
    EGIT_REPOSITORY,
    EGIT_REFERENCE,
    EGIT_COMMIT,
    EGIT_TREE,
    EGIT_BLOB,
    EGIT_TAG,
    EGIT_OBJECT,
    EGIT_SIGNATURE,
    EGIT_BLAME,
    EGIT_CONFIG,
    EGIT_TRANSACTION,
    EGIT_INDEX,
    EGIT_INDEX_ENTRY
} egit_type;

/**
 * Hashable wrapper structure for a git_??? struct.
 * These are used in an internal object hash-table to ensure that pointers aren't freed too early.
 * This is necessary since there may be many emacs_values that point to the same git_repository
 * (for example), so we can't just free the git_repository pointer in a finalizer called from Emacs.
 * Moreover, emacs_values that store e.g. git_object pointers must keep the git_repository alive.
 * To fix this we keep a hash table of pointers mapping to egit_objects, counting references and
 * freeing objects when they reach zero.
 *
 * User-pointers returned to Emacs should always wrap a struct of type egit_object.
 */
typedef struct {
    UT_hash_handle hh;                 /**< For internal use by the hash table. */
    egit_type type;                    /**< Type of object stored. */
    ptrdiff_t refcount;                /**< Reference count. */
    void *ptr;                         /**< Pointer to git_??? structure. */
} egit_object;

/**
 * Return the git object type stored by en Emacs value.
 * @param env The active Emacs environment.
 * @param _obj The value to check.
 * @return The object type, or EGIT_UNKNOWN if not known.
 */
egit_type egit_get_type(emacs_env *env, emacs_value _obj);

/**
 * Assert that an Emacs value represents a libgit2 struct of a given type, or signal an Emacs error.
 * To check for EGIT_OBJECT, use egit_assert_object instead.
 * @param env The active Emacs environment.
 * @param obj The value to check.
 * @param type The type to check for.
 * @param predicate Symbol to use in a wrong-type-argument error signal.
 * @return True iff the value has the right type.
 */
bool egit_assert_type(emacs_env *env, emacs_value obj, egit_type type, emacs_value predicate);

/**
 * Assert that an Emacs value represents a libgit2 git_object struct, or signal an Emacs error.
 * @param env The active Emacs environment.
 * @param obj The value to check.
 * @return True iff the value has the right type.
 */
bool egit_assert_object(emacs_env *env, emacs_value obj);

/**
 * Wrap a git_??? structure in an emacs_value.
 * @param env The active Emacs environment.
 * @param obj The type of the object.
 * @param ptr The pointer to store.
 * @return The Emacs value.
 */
emacs_value egit_wrap(emacs_env *env, egit_type type, void* ptr);

/**
 * If libgit2 signaled an error, dispatch that error to Emacs.
 * @param env The active Emacs environment.
 * @param retval A libgit2 return value.
 * @return True iff an error was signalled.
 */
bool egit_dispatch_error(emacs_env *env, int retval);

/**
 * Define functions visible to Emacs.
 * This function only needs to be called once.
 * @param env The active Emacs environment.
 */
void egit_init(emacs_env *env);

#endif /* EGIT_H */
