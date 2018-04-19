#include <stdarg.h>

#include "emacs-module.h"

#ifndef INTERFACE_H
#define INTERFACE_H

extern emacs_value em_nil, em_stringp, em_t;
extern emacs_value em_git_repository_p;

void em_init(emacs_env *env);

bool em_assert(emacs_env *env, emacs_value predicate, emacs_value arg);
void em_signal_giterr(emacs_env *env, int _klass, const char* _msg);
void em_signal_void(emacs_env *env);
void em_signal_wrong_type(emacs_env *env, emacs_value expected, emacs_value actual);

char *em_get_string(emacs_env *env, emacs_value arg);

emacs_value em_cons(emacs_env *env, emacs_value car, emacs_value cdr);
void em_define_error(emacs_env *env, emacs_value symbol, const char *msg);
void em_defun(emacs_env *env, const char *name, emacs_value func);
void em_provide(emacs_env *env, const char *feature);
bool em_user_ptrp(emacs_env *env, emacs_value val);
emacs_value em_vector_vargs(emacs_env *env, ptrdiff_t nargs, va_list vargs);
emacs_value em_vector(emacs_env *env, ptrdiff_t nargs, ...);

#endif /* INTERFACE_H */
