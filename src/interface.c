#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "emacs-module.h"
#include "interface.h"

#define GLOBREF(val) env->make_global_ref(env, (val))
#define INTERN(val) env->intern(env, (val))

emacs_value em_nil, em_stringp, em_t;
emacs_value em_git_repository_p;

static emacs_value _cons, _defalias, _define_error, _giterr,
    _not_implemented, _provide, _user_ptrp, _vector, _wrong_type_argument;
static bool initialized = false;


void em_init(emacs_env *env)
{
    if (initialized)
        return;

    em_nil = GLOBREF(INTERN("nil"));
    em_stringp = GLOBREF(INTERN("stringp"));
    em_t = GLOBREF(INTERN("t"));

    em_git_repository_p = GLOBREF(INTERN("git-repository-p"));

    _cons = GLOBREF(INTERN("cons"));
    _defalias = GLOBREF(INTERN("defalias"));
    _define_error = GLOBREF(INTERN("define-error"));
    _giterr = GLOBREF(INTERN("giterr"));
    _not_implemented = GLOBREF(INTERN("not-implemented"));
    _provide = GLOBREF(INTERN("provide"));
    _user_ptrp = GLOBREF(INTERN("user-ptrp"));
    _vector = GLOBREF(INTERN("vector"));
    _wrong_type_argument = GLOBREF(INTERN("wrong-type-argument"));

    em_define_error(env, _giterr, "Git error");
    em_define_error(env, _not_implemented, "Not implemented");

    initialized = true;
}

static emacs_value em_funcall(emacs_env *env, emacs_value func, ptrdiff_t nargs, ...)
{
    emacs_value args[nargs];

    va_list vargs;
    va_start(vargs, nargs);
    for (ptrdiff_t i = 0; i < nargs; i++)
        args[i] = va_arg(vargs, emacs_value);
    va_end(vargs);

    return env->funcall(env, func, nargs, args);
}

bool em_assert(emacs_env *env, emacs_value predicate, emacs_value arg)
{
    bool cond = env->is_not_nil(env, em_funcall(env, predicate, 1, arg));
    if (!cond)
        em_signal_wrong_type(env, predicate, arg);
    return cond;
}

void em_signal_giterr(emacs_env *env, int _klass, const char* _msg)
{
    emacs_value klass = env->make_integer(env, _klass);
    emacs_value msg = env->make_string(env, _msg, strlen(_msg));
    env->non_local_exit_signal(env, _giterr, em_cons(env, klass, em_cons(env, msg, em_nil)));
}

void em_signal_void(emacs_env *env)
{
    env->non_local_exit_signal(env, _not_implemented, em_nil);
}

void em_signal_wrong_type(emacs_env *env, emacs_value expected, emacs_value actual)
{
    env->non_local_exit_signal(
        env, _wrong_type_argument,
        em_cons(env, expected, em_cons(env, actual, em_nil))
    );
}


char *em_get_string(emacs_env *env, emacs_value arg)
{
    ptrdiff_t size;
    env->copy_string_contents(env, arg, NULL, &size);

    char *buf = (char*) malloc(size * sizeof(char));
    env->copy_string_contents(env, arg, buf, &size);

    return buf;
}

emacs_value em_cons(emacs_env *env, emacs_value car, emacs_value cdr)
{
    return em_funcall(env, _cons, 2, car, cdr);
}

void em_define_error(emacs_env *env, emacs_value symbol, const char *msg)
{
    em_funcall(env, _define_error, 2, symbol, env->make_string(env, msg, strlen(msg)));
}

void em_defun(emacs_env *env, const char *name, emacs_value func)
{
    em_funcall(env, _defalias, 2, INTERN(name), func);
}

void em_provide(emacs_env *env, const char *feature)
{
    em_funcall(env, _provide, 1, INTERN(feature));
}

bool em_user_ptrp(emacs_env *env, emacs_value val)
{
    return env->is_not_nil(env, em_funcall(env, _user_ptrp, 1, val));
}

emacs_value em_vector_vargs(emacs_env *env, ptrdiff_t nargs, va_list vargs)
{
    emacs_value args[nargs];
    for (ptrdiff_t i = 0; i < nargs; i++)
        args[i] = va_arg(vargs, emacs_value);
    return env->funcall(env, _vector, nargs, args);
}

emacs_value em_vector(emacs_env *env, ptrdiff_t nargs, ...)
{
    va_list vargs;
    va_start(vargs, nargs);
    emacs_value retval = em_vector_vargs(env, nargs, vargs);
    va_end(vargs);
    return retval;
}
