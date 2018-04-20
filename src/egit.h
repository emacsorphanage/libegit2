#include "emacs-module.h"
#include "uthash.h"

#ifndef EGIT_H
#define EGIT_H

#define EGIT_DOC(name, args, docstring)                                 \
    const char *egit_##name##__doc = (docstring "\n\n(fn " args ")")

#define EGIT_DEFUN(name, ...)                                   \
    extern const char *egit_##name##__doc;                      \
    emacs_value egit_##name(emacs_env *env, __VA_ARGS__)        \

#define EGIT_ASSERT_STRING(val)                                         \
    do { if (!em_assert(env, em_stringp, (val))) return em_nil; } while (0)
#define EGIT_ASSERT_OBJECT(val)                                         \
    do { if (!egit_assert_object(env, (val))) return em_nil; } while (0)
#define EGIT_ASSERT_REPOSITORY(val)                                     \
    do { if (!egit_assert_type(env, (val), EGIT_REPOSITORY, em_git_repository_p)) return em_nil; } while (0)
#define EGIT_ASSERT_REFERENCE(val)                                      \
    do { if (!egit_assert_type(env, (val), EGIT_REFERENCE, em_git_reference_p)) return em_nil; } while (0)
#define EGIT_EXTRACT(val) (((egit_object*)env->get_user_ptr(env, (val)))->ptr)
#define EGIT_CHECK_ERROR(val)                                           \
    do { if (egit_dispatch_error(env, (val))) return em_nil; } while (0)

#define EGIT_RET_BUF_AS_STRING(buf)                                     \
    do {                                                                \
        emacs_value ret = env->make_string(env, (buf).ptr, (buf).size); \
        git_buf_free(&(buf));                                           \
        return ret;                                                     \
    } while (0)

typedef enum {
    EGIT_UNKNOWN,
    EGIT_REPOSITORY,
    EGIT_REFERENCE,
    EGIT_COMMIT,
    EGIT_TREE,
    EGIT_BLOB,
    EGIT_TAG,
    EGIT_OBJECT,
} egit_type;

typedef struct {
    UT_hash_handle hh;
    egit_type type;
    ptrdiff_t refcount;
    void *ptr;
} egit_object;

egit_type egit_get_type(emacs_env *env, emacs_value _obj);
bool egit_assert_type(emacs_env *env, emacs_value obj, egit_type type, emacs_value predicate);
bool egit_assert_object(emacs_env *env, emacs_value obj);

void egit_decref_wrapped(void *obj);
void egit_decref_wrapper(void *obj);
emacs_value egit_wrap(emacs_env *env, egit_type type, void* ptr);

emacs_value egit_dispatch_1(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value egit_dispatch_2(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

bool egit_dispatch_error(emacs_env *env, int retval);

void egit_init(emacs_env *env);

#endif /* EGIT_H */
