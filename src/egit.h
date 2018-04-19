#include "emacs-module.h"
#include "uthash.h"

#ifndef EGIT_H
#define EGIT_H

typedef enum {
    EGIT_REPOSITORY
} egit_type;

typedef struct {
    UT_hash_handle hh;
    egit_type type;
    ptrdiff_t refcount;
    void *ptr;
} egit_object;

void egit_init(emacs_env *env);

emacs_value egit_wrap(emacs_env *env, egit_type type, void* ptr);

emacs_value egit_dispatch_1(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value egit_dispatch_2(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

bool egit_dispatch_error(emacs_env *env, int retval);

#endif /* EGIT_H */
