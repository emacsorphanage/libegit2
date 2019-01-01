#include "emacs-module.h"
#include "git2.h"
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

// Assert that VAL is a git annotated commit, signal an error and return otherwise.
#define EGIT_ASSERT_ANNOTATED_COMMIT(val)                                          \
    do { if (!egit_assert_type(env, (val), EGIT_ANNOTATED_COMMIT, esym_libgit_annotated_commit_p)) return esym_nil; } while (0)

// Assert that VAL is a git blame, signal an error and return otherwise.
#define EGIT_ASSERT_BLAME(val)                                          \
    do { if (!egit_assert_type(env, (val), EGIT_BLAME, esym_libgit_blame_p)) return esym_nil; } while (0)

// Assert that VAL is a git blob, signal an error and return otherwise.
#define EGIT_ASSERT_BLOB(val)                                           \
    do { if (!egit_assert_type(env, (val), EGIT_BLOB, esym_libgit_blob_p)) return esym_nil; } while (0)

// Assert that VAL is a git blame hunk, signal an error and return otherwise.
#define EGIT_ASSERT_BLAME_HUNK(val)                                     \
    do { if (!egit_assert_type(env, (val), EGIT_BLAME_HUNK, esym_libgit_blame_hunk_p)) return esym_nil; } while (0)

// Assert that VAL is a git commit, signal an error and return otherwise.
#define EGIT_ASSERT_COMMIT(val)                                         \
    do { if (!egit_assert_type(env, (val), EGIT_COMMIT, esym_libgit_commit_p)) return esym_nil; } while (0)

// Assert that VAL is a git config, signal an error and return otherwise.
#define EGIT_ASSERT_CONFIG(val)                                         \
    do { if (!egit_assert_type(env, (val), EGIT_CONFIG, esym_libgit_config_p)) return esym_nil; } while (0)

// Assert that VAL is a git credential, signal an error and return otherwise.
#define EGIT_ASSERT_CRED(val)                                           \
    do { if (!egit_assert_type(env, (val), EGIT_CRED, esym_libgit_cred_p)) return esym_nil; } while (0)

// Assert that VAL is a git diff, signal an error and return otherwise.
#define EGIT_ASSERT_DIFF(val)                                           \
    do { if (!egit_assert_type(env, (val), EGIT_DIFF, esym_libgit_diff_p)) return esym_nil; } while (0)

// Assert that VAL is a git diff delta, signal an error and return otherwise.
#define EGIT_ASSERT_DIFF_DELTA(val)                                     \
    do { if (!egit_assert_type(env, (val), EGIT_DIFF_DELTA, esym_libgit_diff_delta_p)) return esym_nil; } while (0)

// Assert that VAL is a git diff binary, signal an error and return otherwise.
#define EGIT_ASSERT_DIFF_BINARY(val)                                    \
    do { if (!egit_assert_type(env, (val), EGIT_DIFF_BINARY, esym_libgit_diff_binary_p)) return esym_nil; } while (0)

// Assert that VAL is a git diff hunk, signal an error and return otherwise.
#define EGIT_ASSERT_DIFF_HUNK(val)                                     \
    do { if (!egit_assert_type(env, (val), EGIT_DIFF_HUNK, esym_libgit_diff_hunk_p)) return esym_nil; } while (0)

// Assert that VAL is a git diff line, signal an error and return otherwise.
#define EGIT_ASSERT_DIFF_LINE(val)                                     \
    do { if (!egit_assert_type(env, (val), EGIT_DIFF_LINE, esym_libgit_diff_line_p)) return esym_nil; } while (0)

// Assert that VAL is a git index, signal an error and return otherwise.
#define EGIT_ASSERT_INDEX(val)                                          \
    do { if (!egit_assert_type(env, (val), EGIT_INDEX, esym_libgit_index_p)) return esym_nil; } while (0)

// Assert that VAL is a git index entry, signal an error and return otherwise.
#define EGIT_ASSERT_INDEX_ENTRY(val)                                    \
    do { if (!egit_assert_type(env, (val), EGIT_INDEX_ENTRY, esym_libgit_index_entry_p)) return esym_nil; } while (0)

// Assert that VAL is a git object, signal an error and return otherwise.
#define EGIT_ASSERT_OBJECT(val)                                         \
    do { if (!egit_assert_object(env, (val))) return esym_nil; } while (0)

// Assert that VAL is a git pathspec, signal an error and return otherwise.
#define EGIT_ASSERT_PATHSPEC(val)                                      \
    do { if (!egit_assert_type(env, (val), EGIT_PATHSPEC, esym_libgit_pathspec_p)) return esym_nil; } while (0)

// Assert that VAL is a git pathspec match list, signal an error and return otherwise.
#define EGIT_ASSERT_PATHSPEC_MATCH_LIST(val)                                      \
    do { if (!egit_assert_type(env, (val), EGIT_PATHSPEC_MATCH_LIST, esym_libgit_pathspec_match_list_p)) return esym_nil; } while (0)

// Assert that VAL is a git reference, signal an error and return otherwise.
#define EGIT_ASSERT_REFERENCE(val)                                      \
    do { if (!egit_assert_type(env, (val), EGIT_REFERENCE, esym_libgit_reference_p)) return esym_nil; } while (0)

// Assert that VAL is a reflog, signal an error and return otherwise.
#define EGIT_ASSERT_REFLOG(val)                                         \
    do { if (!egit_assert_type(env, (val), EGIT_REFLOG, esym_libgit_reflog_p)) return esym_nil; } while (0)

// Assert that VAL is a reflog entry, signal an error and return otherwise.
#define EGIT_ASSERT_REFLOG_ENTRY(val)                                   \
    do { if (!egit_assert_type(env, (val), EGIT_REFLOG_ENTRY, esym_libgit_reflog_entry_p)) return esym_nil; } while (0)

// Assert that VAL is a git refspec, signal an error and return otherwise.
#define EGIT_ASSERT_REFSPEC(val)                                        \
    do { if (!egit_assert_type(env, (val), EGIT_REFSPEC, esym_libgit_refspec_p)) return esym_nil; } while (0)

// Assert that VAL is a git remote, signal an error and return otherwise.
#define EGIT_ASSERT_REMOTE(val)                                         \
    do { if (!egit_assert_type(env, (val), EGIT_REMOTE, esym_libgit_remote_p)) return esym_nil; } while (0)

// Assert that VAL is a git repository, signal an error and return otherwise.
#define EGIT_ASSERT_REPOSITORY(val)                                     \
    do { if (!egit_assert_type(env, (val), EGIT_REPOSITORY, esym_libgit_repository_p)) return esym_nil; } while (0)

// Assert that VAL is a git revwalk, signal an error and return otherwise.
#define EGIT_ASSERT_REVWALK(val)                                        \
    do { if (!egit_assert_type(env, (val), EGIT_REVWALK, esym_libgit_revwalk_p)) return esym_nil; } while (0)

// Assert that VAL is a signature, signal an error and return otherwise.
#define EGIT_ASSERT_SIGNATURE(val)                                      \
    do { if (!egit_assert_type(env, (val), EGIT_SIGNATURE, esym_libgit_signature_p)) return esym_nil; } while (0)

// Assert that VAL is a transaction, signal an error and return otherwise.
#define EGIT_ASSERT_SIGNATURE_OR_NIL(val)                               \
    do { if (EGIT_EXTRACT_BOOLEAN(val)) EGIT_ASSERT_SIGNATURE(val); } while (0)

// Assert that VAL is a signature, signal an error and return otherwise.
#define EGIT_ASSERT_SUBMODULE(val)                                      \
    do { if (!egit_assert_type(env, (val), EGIT_SUBMODULE, esym_libgit_submodule_p)) return esym_nil; } while (0)

// Assert that VAL is a transaction, signal an error and return otherwise.
#define EGIT_ASSERT_TAG(val)                                            \
    do { if (!egit_assert_type(env, (val), EGIT_TAG, esym_libgit_tag_p)) return esym_nil; } while (0)

// Assert that VAL is a transaction, signal an error and return otherwise.
#define EGIT_ASSERT_TRANSACTION(val)                                    \
    do { if (!egit_assert_type(env, (val), EGIT_TRANSACTION, esym_libgit_transaction_p)) return esym_nil; } while (0)

// Assert that VAL is a tree, signal an error and return otherwise.
#define EGIT_ASSERT_TREE(val)                                           \
    do { if (!egit_assert_type(env, (val), EGIT_TREE, esym_libgit_tree_p)) return esym_nil; } while (0)

// Assert that VAL is a treebuilder, signal an error and return otherwise.
#define EGIT_ASSERT_TREEBUILDER(val)                                    \
    do { if (!egit_assert_type(env, (val), EGIT_TREEBUILDER, esym_libgit_treebuilder_p)) return esym_nil; } while (0)

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
 * Extract egit_object pointer representing a parent object, or NULL.
 * Caller is responsible for ensuring that this is a valid operation.
 */
#define EGIT_EXTRACT_PARENT(val)                        \
    (((egit_object*) EM_EXTRACT_USER_PTR(val))->parent) \

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
    do { if (egit_dispatch_error(env, (val))) return esym_nil; } while (0)

/**
 * Convert a git_buf to an Emacs string and return it, freeing the git_buf in the process.
 */
#define EGIT_RET_BUF_AS_STRING(buf)                                     \
    do {                                                                \
        emacs_value ret = env->make_string(env, (buf).ptr, (buf).size); \
        git_buf_dispose(&(buf));                                        \
        return ret;                                                     \
    } while (0)

/**
 * Convert a git_strarray to an Emacs list and return it, freeing the git_strarray in the process.
 */
#define EGIT_RET_STRARRAY(arr)                                  \
    do {                                                        \
        emacs_value list = esym_nil;                              \
        for (ptrdiff_t c = (arr).count-1; c >= 0; c--) {        \
            emacs_value str = EM_STRING((arr).strings[c]);      \
            list = em_cons(env, str, list);                     \
        }                                                       \
        git_strarray_free(&(arr));                              \
        return list;                                            \
    } while (0)

/**
 * Set or unset a bit according to the truth value of an emacs value.
 * @param tgt The value to change
 * @param bit The bit to flip
 * @param opt The determining emacs value
 */
#define EGIT_SET_BIT(tgt, bit, opt)                      \
    do {                                                 \
        if (EM_EXTRACT_BOOLEAN(opt))                     \
            (tgt) |= (bit);                              \
        else                                             \
            (tgt) &= ~(bit);                             \
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
    EGIT_BLAME_HUNK,
    EGIT_CONFIG,
    EGIT_TRANSACTION,
    EGIT_INDEX,
    EGIT_INDEX_ENTRY,
    EGIT_DIFF,
    EGIT_DIFF_DELTA,
    EGIT_DIFF_BINARY,
    EGIT_DIFF_HUNK,
    EGIT_DIFF_LINE,
    EGIT_PATHSPEC,
    EGIT_PATHSPEC_MATCH_LIST,
    EGIT_REMOTE,
    EGIT_REFSPEC,
    EGIT_SUBMODULE,
    EGIT_CRED,
    EGIT_ANNOTATED_COMMIT,
    EGIT_REFLOG,
    EGIT_REFLOG_ENTRY,
    EGIT_REVWALK,
    EGIT_TREEBUILDER
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
typedef struct egit_object_s egit_object;

struct egit_object_s {
    egit_type type;             /**< Type of object stored. */
    ptrdiff_t refcount;         /**< Reference count. */
    void *ptr;                  /**< Pointer to git_??? structure. */
    egit_object *parent;        /**< Optional pointer to parent wrapper. */
};

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
 * Signal a wrong-type-argument error if ARG is not a proper list, every
 * element of which satisfies has the correct libgit2 type.
 * @param env The active Emacs environment.
 * @param type The type to check for.
 * @param predicate Symbol to use in a wrong-type-argument error signal.
 * @param arg The list.
 * @return The number of elements in the list, or negative if error.
 */
ptrdiff_t egit_assert_list(emacs_env *env, egit_type type, emacs_value predicate, emacs_value arg);

/**
 * Finalizer for user pointers.
 */
void egit_finalize(void* _obj);

/**
 * Wrap a git_??? structure in an emacs_value.
 * @param env The active Emacs environment.
 * @param obj The type of the object.
 * @param ptr The pointer to store.
 * @return The Emacs value.
 */
emacs_value egit_wrap(emacs_env *env, egit_type type, const void* ptr, egit_object *parent);

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
