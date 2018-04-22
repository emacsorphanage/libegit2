#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "emacs-module.h"
#include "interface.h"

#define GLOBREF(val) env->make_global_ref(env, (val))
#define INTERN(val) env->intern(env, (val))

// We store some globa references to emacs objects, mostly symbols,
// so that we don't have to waste time calling intern later on.

emacs_value em_nil, em_stringp, em_t;

// Git object predicates
emacs_value em_libgit_object_p, em_libgit_repository_p, em_libgit_reference_p;
emacs_value em_repository, em_reference, em_commit, em_tree, em_blob, em_tag, em_object;

// Repository states
emacs_value em_merge, em_revert, em_revert_sequence, em_cherrypick,
    em_cherrypick_sequence, em_bisect, em_rebase, em_rebase_interactive, em_rebase_merge,
    em_apply_mailbox, em_apply_mailbox_or_rebase;

// Reference types
emacs_value em_direct, em_symbolic;

// Symbols that are only reachable from within this file.
static emacs_value _cons, _defalias, _define_error, _expand_file_name, _giterr,
    _not_implemented, _provide, _user_ptrp, _vector, _wrong_type_argument,
    _wrong_value_argument;


void em_init(emacs_env *env)
{
    em_nil = GLOBREF(INTERN("nil"));
    em_stringp = GLOBREF(INTERN("stringp"));
    em_t = GLOBREF(INTERN("t"));

    em_libgit_object_p = GLOBREF(INTERN("libgit-object-p"));
    em_libgit_repository_p = GLOBREF(INTERN("libgit-repository-p"));
    em_libgit_reference_p = GLOBREF(INTERN("libgit-reference-p"));

    em_repository = GLOBREF(INTERN("repository"));
    em_reference = GLOBREF(INTERN("reference"));
    em_commit = GLOBREF(INTERN("commit"));
    em_tree = GLOBREF(INTERN("tree"));
    em_blob = GLOBREF(INTERN("blob"));
    em_tag = GLOBREF(INTERN("tag"));
    em_object = GLOBREF(INTERN("object"));

    em_merge = GLOBREF(INTERN("merge"));
    em_revert = GLOBREF(INTERN("revert"));
    em_revert_sequence = GLOBREF(INTERN("revert-sequence"));
    em_cherrypick = GLOBREF(INTERN("cherrypick"));
    em_cherrypick_sequence = GLOBREF(INTERN("cherrypick-sequence"));
    em_bisect = GLOBREF(INTERN("bisect"));
    em_rebase = GLOBREF(INTERN("rebase"));
    em_rebase_interactive = GLOBREF(INTERN("rebase-interactive"));
    em_rebase_merge = GLOBREF(INTERN("rebase-merge"));
    em_apply_mailbox = GLOBREF(INTERN("apply-mailbox"));
    em_apply_mailbox_or_rebase = GLOBREF(INTERN("apply-mailbox-or-rebase"));

    em_direct = GLOBREF(INTERN("direct"));
    em_symbolic = GLOBREF(INTERN("symbolic"));

    _cons = GLOBREF(INTERN("cons"));
    _defalias = GLOBREF(INTERN("defalias"));
    _define_error = GLOBREF(INTERN("define-error"));
    _expand_file_name = GLOBREF(INTERN("expand-file-name"));
    _giterr = GLOBREF(INTERN("giterr"));
    _not_implemented = GLOBREF(INTERN("not-implemented"));
    _provide = GLOBREF(INTERN("provide"));
    _user_ptrp = GLOBREF(INTERN("user-ptrp"));
    _vector = GLOBREF(INTERN("vector"));
    _wrong_type_argument = GLOBREF(INTERN("wrong-type-argument"));
    _wrong_value_argument = GLOBREF(INTERN("wrong-value-argument"));

    em_define_error(env, _giterr, "Git error");
    em_define_error(env, _not_implemented, "Not implemented");
    em_define_error(env, _wrong_value_argument, "Wrong argument value passed");
}

/**
 * Call an Emacs function without error checking.
 * @param env The active Emacs environment.
 * @param func The function to call.
 * @param nargs The number of arguments that follow.
 * @return The function return value.
 */
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

void em_signal_wrong_type(emacs_env *env, emacs_value expected, emacs_value actual)
{
    env->non_local_exit_signal(
        env, _wrong_type_argument,
        em_cons(env, expected, em_cons(env, actual, em_nil))
    );
}

void em_signal_wrong_value(emacs_env *env, emacs_value actual)
{
    env->non_local_exit_signal(env, _wrong_value_argument, actual);
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

emacs_value em_expand_file_name(emacs_env *env, emacs_value path)
{
    return em_funcall(env, _expand_file_name, 1, path);
}

void em_provide(emacs_env *env, const char *feature)
{
    em_funcall(env, _provide, 1, INTERN(feature));
}

bool em_user_ptrp(emacs_env *env, emacs_value val)
{
    return env->is_not_nil(env, em_funcall(env, _user_ptrp, 1, val));
}
