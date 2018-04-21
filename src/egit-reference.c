#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


// =============================================================================
// Constructors

EGIT_DOC(reference_create, "REPO NAME ID &optional FORCE LOG-MESSAGE",
         "Create a new direct reference.");
emacs_value egit_reference_create(
    emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _id,
    emacs_value _force, emacs_value _log_message)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_name);
    EGIT_ASSERT_STRING(_id);
    EGIT_ASSERT_STRING_OR_NIL(_log_message);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid id;
    EGIT_EXTRACT_OID(_id, id);
    int force = EGIT_EXTRACT_BOOLEAN(_force);
    git_reference *ref;
    int retval;
    {
        char *name = EGIT_EXTRACT_STRING(_name);
        char *log_message = EGIT_EXTRACT_STRING_OR_NULL(_log_message);
        retval = git_reference_create(&ref, repo, name, &id, force, log_message);
        free(name);
        EGIT_FREE(log_message);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref);
}

EGIT_DOC(reference_create_matching, "REPO NAME ID &optional FORCE CURRENT-ID LOG-MESSAGE",
         "Conditionally create a new direct reference.");
emacs_value egit_reference_create_matching(
    emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _id,
    emacs_value _force, emacs_value _current_id, emacs_value _log_message)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_name);
    EGIT_ASSERT_STRING(_id);
    EGIT_ASSERT_STRING_OR_NIL(_current_id);
    EGIT_ASSERT_STRING_OR_NIL(_log_message);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid id, current_id;
    EGIT_EXTRACT_OID(_id, id);
    if (EGIT_EXTRACT_BOOLEAN(_current_id))
        EGIT_EXTRACT_OID(_current_id, current_id);
    int force = EGIT_EXTRACT_BOOLEAN(_force);
    git_reference *ref;
    int retval;
    {
        char *name = EGIT_EXTRACT_STRING(_name);
        char *log_message = EGIT_EXTRACT_STRING_OR_NULL(_log_message);
        retval = git_reference_create_matching(
            &ref, repo, name, &id, force,
            EGIT_EXTRACT_BOOLEAN(_current_id) ? &current_id : NULL,
            log_message
        );
        free(name);
        EGIT_FREE(log_message);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref);
}

EGIT_DOC(reference_lookup, "REPO NAME", "Lookup a reference by NAME in REPO.");
emacs_value egit_reference_lookup(emacs_env *env, emacs_value _repo, emacs_value _name)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_name);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_reference *ref;
    int retval;
    {
        char *name = EGIT_EXTRACT_STRING(_name);
        retval = git_reference_lookup(&ref, repo, name);
        free(name);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref);
}


// =============================================================================
// Operations

EGIT_DOC(reference_delete, "REF", "Delete an existing reference.");
emacs_value egit_reference_delete(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_reference_delete(ref);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}


// =============================================================================
// Getters

EGIT_DOC(reference_name, "REF", "Return the full name for the REF.");
emacs_value egit_reference_name(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    const char *name = git_reference_name(ref);
    return env->make_string(env, name, strlen(name));
}

EGIT_DOC(reference_owner, "REF", "Return the repository that REF belongs to.");
emacs_value egit_reference_owner(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    git_repository *repo = git_reference_owner(ref);
    return egit_wrap(env, EGIT_REPOSITORY, repo);
}

EGIT_DOC(reference_resolve, "REF",
         "Iteratively peel REF until it resolves directly to an OID.");
emacs_value egit_reference_resolve(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    git_reference *newref;
    int retval = git_reference_resolve(&newref, ref);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_REFERENCE, newref);
}

EGIT_DOC(reference_target, "REF",
         "Return the OID pointed to by REF, or nil if REF is not direct");
emacs_value egit_reference_target(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    const git_oid *oid = git_reference_target(ref);
    if (!oid) return em_nil;
    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}
