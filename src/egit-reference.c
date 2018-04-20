#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


// =============================================================================
// Getters

emacs_value egit_reference_name(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    const char *name = git_reference_name(ref);
    return env->make_string(env, name, strlen(name));
}

emacs_value egit_reference_owner(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    git_repository *repo = git_reference_owner(ref);
    return egit_wrap(env, EGIT_REPOSITORY, repo);
}

emacs_value egit_reference_resolve(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    git_reference *newref;
    int retval = git_reference_resolve(&newref, ref);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_REFERENCE, newref);
}

emacs_value egit_reference_target(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    const git_oid *oid = git_reference_target(ref);
    if (!oid) return em_nil;
    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}


// =============================================================================
// Predicates

emacs_value egit_reference_p(emacs_env *env, emacs_value obj)
{
    return egit_get_type(env, obj) == EGIT_REFERENCE ? em_t : em_nil;
}
