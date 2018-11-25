#include "egit.h"

#ifndef EGIT_UTIL_H
#define EGIT_UTIL_H

typedef struct {
    emacs_env *env;
    emacs_value func;
} egit_generic_payload;

bool egit_strarray_from_list(git_strarray *array, emacs_env *env, emacs_value list);
void egit_strarray_dispose(git_strarray *array);

int egit_cred_dup(git_cred **out, git_cred *cred);

#endif /* EGIT_UTIL_H */
