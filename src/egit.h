#include "emacs-module.h"
#include "uthash.h"

#ifndef EGIT_H
#define EGIT_H

typedef enum {
    EGIT_UNKNOWN,
    EGIT_REPOSITORY
} egit_type;

typedef struct {
    UT_hash_handle hh;
    egit_type type;
    ptrdiff_t refcount;
    void *ptr;
} egit_object;

egit_type egit_get_type(emacs_env *env, emacs_value _obj);
bool egit_assert_type(emacs_env *env, emacs_value obj, egit_type type, emacs_value predicate);
emacs_value egit_wrap(emacs_env *env, egit_type type, void* ptr);
void *egit_extract(emacs_env *env, emacs_value _obj);

emacs_value egit_dispatch_1(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value egit_dispatch_2(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

bool egit_dispatch_error(emacs_env *env, int retval);

void egit_init(emacs_env *env);

#endif /* EGIT_H */
