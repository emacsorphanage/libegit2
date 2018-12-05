#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
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
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_id);
    EM_ASSERT_STRING_OR_NIL(_log_message);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid id;
    EGIT_EXTRACT_OID(_id, id);
    int force = EM_EXTRACT_BOOLEAN(_force);
    git_reference *ref;
    int retval;
    {
        char *name = EM_EXTRACT_STRING(_name);
        char *log_message = EM_EXTRACT_STRING_OR_NULL(_log_message);
        retval = git_reference_create(&ref, repo, name, &id, force, log_message);
        free(name);
        free(log_message);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(reference_create_matching, "REPO NAME ID &optional FORCE CURRENT-ID LOG-MESSAGE",
         "Conditionally create a new direct reference.");
emacs_value egit_reference_create_matching(
    emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _id,
    emacs_value _force, emacs_value _current_id, emacs_value _log_message)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_id);
    EM_ASSERT_STRING_OR_NIL(_current_id);
    EM_ASSERT_STRING_OR_NIL(_log_message);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid id, current_id;
    EGIT_EXTRACT_OID(_id, id);
    if (EM_EXTRACT_BOOLEAN(_current_id))
        EGIT_EXTRACT_OID(_current_id, current_id);
    int force = EM_EXTRACT_BOOLEAN(_force);
    git_reference *ref;
    int retval;
    {
        char *name = EM_EXTRACT_STRING(_name);
        char *log_message = EM_EXTRACT_STRING_OR_NULL(_log_message);
        retval = git_reference_create_matching(
            &ref, repo, name, &id, force,
            EM_EXTRACT_BOOLEAN(_current_id) ? &current_id : NULL,
            log_message
        );
        free(name);
        free(log_message);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(reference_dup, "REF", "Duplicate an existing reference.");
emacs_value egit_reference_dup(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    git_reference *new_ref;
    int retval = git_reference_dup(&new_ref, ref);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_REFERENCE, new_ref, EGIT_EXTRACT_PARENT(_ref));
}

EGIT_DOC(reference_dwim, "REPO SHORTHAND", "Lookup a reference by DWIMing its short name.");
emacs_value egit_reference_dwim(emacs_env *env, emacs_value _repo, emacs_value _shorthand)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_shorthand);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_reference *ref;
    int retval;
    {
        char *shorthand = EM_EXTRACT_STRING(_shorthand);
        retval = git_reference_dwim(&ref, repo, shorthand);
        free(shorthand);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(reference_lookup, "REPO NAME", "Lookup a reference by NAME in REPO.");
emacs_value egit_reference_lookup(emacs_env *env, emacs_value _repo, emacs_value _name)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_reference *ref;
    int retval;
    {
        char *name = EM_EXTRACT_STRING(_name);
        retval = git_reference_lookup(&ref, repo, name);
        free(name);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref, EM_EXTRACT_USER_PTR(_repo));
}


// =============================================================================
// Getters

EGIT_DOC(reference_list, "REPO", "Get a list of all reference names in REPO.");
emacs_value egit_reference_list(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_strarray out = {NULL, 0};
    int retval = git_reference_list(&out, repo);
    EGIT_CHECK_ERROR(retval);

    EGIT_RET_STRARRAY(out);
}

EGIT_DOC(reference_name, "REF", "Return the full name for the REF.");
emacs_value egit_reference_name(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    const char *name = git_reference_name(ref);
    return EM_STRING(name);
}

EGIT_DOC(reference_name_to_id, "REPO REFNAME",
         "Lookup a reference by name and resolve immediately to OID");
emacs_value egit_reference_name_to_id(emacs_env *env, emacs_value _repo, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    int retval;
    {
        char *refname = EM_EXTRACT_STRING(_refname);
        retval = git_reference_name_to_id(&oid, repo, refname);
        free(refname);
    }
    EGIT_CHECK_ERROR(retval);

    const char *oid_s = git_oid_tostr_s(&oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(reference_owner, "REF", "Return the repository that REF belongs to.");
emacs_value egit_reference_owner(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    egit_object *owner = EGIT_EXTRACT_PARENT(_ref);
    owner->refcount++;
    return EM_USER_PTR(owner, egit_finalize);
}

EGIT_DOC(reference_peel, "REF &optional TYPE",
         "Recursively peel REF until an object of type TYPE is found.\n\n"
         "TYPE may be any of `commit', `tree', `blob', `tag' or `nil'\n"
         "(meaning any type).");
emacs_value egit_reference_peel(emacs_env *env, emacs_value _ref, emacs_value _type)
{
    EGIT_ASSERT_REFERENCE(_ref);

    git_otype type;
    if (!em_findsym_otype(&type, env, _type, true))
        return esym_nil;

    git_reference *ref = EGIT_EXTRACT(_ref);
    git_object *obj;
    int retval = git_reference_peel(&obj, ref, type);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_OBJECT, obj, EGIT_EXTRACT_PARENT(_ref));
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
    return egit_wrap(env, EGIT_REFERENCE, newref, EGIT_EXTRACT_PARENT(_ref));
}

EGIT_DOC(reference_shorthand, "REF", "Get the short name of REF.");
emacs_value egit_reference_shorthand(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    const char *shortname = git_reference_shorthand(ref);
    return EM_STRING(shortname);
}

EGIT_DOC(reference_symbolic_target, "REF",
         "Get the full name to the reference pointed to by a symbolic reference, or nil.");
emacs_value egit_reference_symbolic_target(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    const char *name = git_reference_symbolic_target(ref);
    return EM_STRING(name);
}

EGIT_DOC(reference_target, "REF",
         "Return the OID pointed to by REF, or nil if REF is not direct");
emacs_value egit_reference_target(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    const git_oid *oid = git_reference_target(ref);
    if (!oid) return esym_nil;
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(reference_target_peel, "REF",
         "Return the peeled OID pointed to by REF, or nil.");
emacs_value egit_reference_target_peel(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    const git_oid *oid = git_reference_target_peel(ref);
    if (!oid) return esym_nil;
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(reference_type, "REF", "Get the type of REF, either `direct' or `symbolic'.");
emacs_value egit_reference_type(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    git_ref_t type = git_reference_type(ref);
    return type == GIT_REF_OID ? esym_direct : esym_symbolic;
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
    return esym_nil;
}

EGIT_DOC(reference_ensure_log, "REPO REFNAME",
         "Ensure there is a reflog for a particular reference");
emacs_value egit_reference_ensure_log(emacs_env *env, emacs_value _repo, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval;
    {
        char *refname = EM_EXTRACT_STRING(_refname);
        retval = git_reference_ensure_log(repo, refname);
        free(refname);
    }
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(reference_remove, "REF", "Remove an existing reference by name.");
emacs_value egit_reference_remove(emacs_env *env, emacs_value _repo, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval;
    {
        char *refname = EM_EXTRACT_STRING(_refname);
        retval = git_reference_remove(repo, refname);
        free(refname);
    }
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}


// =============================================================================
// Predicates

EGIT_DOC(reference_branch_p, "REF", "Check if REF is a local branch.");
emacs_value egit_reference_branch_p(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_reference_is_branch(ref);
    return retval ? esym_t : esym_nil;
}

EGIT_DOC(reference_direct_p, "REF", "Non-nil if REF is direct.");
emacs_value egit_reference_direct_p(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    git_ref_t type = git_reference_type(ref);
    return type == GIT_REF_OID ? esym_t : esym_nil;
}

EGIT_DOC(reference_has_log_p, "REPO REFNAME",
         "Check if a reflog exists for a particular reference.");
emacs_value egit_reference_has_log_p(emacs_env *env, emacs_value _repo, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval;
    {
        char *refname = EM_EXTRACT_STRING(_refname);
        retval = git_reference_has_log(repo, refname);
        free(refname);
    }
    EGIT_CHECK_ERROR(retval);

    return retval ? esym_t : esym_nil;
}

EGIT_DOC(reference_note_p, "REF", "Check if REF is a note.");
emacs_value egit_reference_note_p(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_reference_is_note(ref);
    return retval ? esym_t : esym_nil;
}

EGIT_DOC(reference_remote_p, "REF", "Check if REF is a remote tracking branch.");
emacs_value egit_reference_remote_p(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_reference_is_remote(ref);
    return retval ? esym_t : esym_nil;
}

EGIT_DOC(reference_symbolic_p, "REF", "Non-nil if REF is symbolic.");
emacs_value egit_reference_symbolic_p(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    git_ref_t type = git_reference_type(ref);
    return type == GIT_REF_SYMBOLIC ? esym_t : esym_nil;
}

EGIT_DOC(reference_tag_p, "REF", "Check if REF is a tag.");
emacs_value egit_reference_tag_p(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_reference_is_tag(ref);
    return retval ? esym_t : esym_nil;
}

EGIT_DOC(reference_valid_name_p, "REFNAME", "Check if a reference name is well-formed.");
emacs_value egit_reference_valid_name_p(emacs_env *env, emacs_value _refname)
{
    EM_ASSERT_STRING(_refname);

    int retval;
    {
        char *refname = EM_EXTRACT_STRING(_refname);
        retval = git_reference_is_valid_name(refname);
        free(refname);
    }

    return retval ? esym_t : esym_nil;
}


// =============================================================================
// Foreach

static int name_callback(const char *name, void *payload)
{
    egit_generic_payload *ctx = (egit_generic_payload*) payload;
    emacs_env *env = ctx->env;

    emacs_value arg = EM_STRING(name);
    env->funcall(env, ctx->func, 1, &arg);

    EM_RETURN_IF_NLE(GIT_EUSER);
    return 0;
}

static int ref_callback(git_reference *ref, void *payload)
{
    egit_generic_payload *ctx = (egit_generic_payload*) payload;
    emacs_env *env = ctx->env;

    emacs_value arg = egit_wrap(env, EGIT_REFERENCE, ref, ctx->parent);
    env->funcall(env, ctx->func, 1, &arg);

    EM_RETURN_IF_NLE(GIT_EUSER);
    return 0;
}

EGIT_DOC(reference_foreach, "REPO FUNC", "Call FUNC for every reference in REPO.");
emacs_value egit_reference_foreach(emacs_env *env, emacs_value _repo, emacs_value func)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_FUNCTION(func);

    git_repository *repo = EGIT_EXTRACT(_repo);
    egit_generic_payload ctx = {.env = env, .func = func, .parent = EM_EXTRACT_USER_PTR(_repo)};
    int retval = git_reference_foreach(repo, &ref_callback, &ctx);

    EM_RETURN_NIL_IF_NLE();
    if (retval == GIT_EUSER)
        return esym_nil;
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(reference_foreach_glob, "REPO GLOB FUNC",
         "Call FUNC for every reference name in REPO that matches GLOB.");
emacs_value egit_reference_foreach_glob(emacs_env *env, emacs_value _repo, emacs_value _glob, emacs_value func)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_glob);
    EM_ASSERT_FUNCTION(func);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *glob = EM_EXTRACT_STRING(_glob);
    egit_generic_payload ctx = {.env = env, .func = func, .parent = NULL};
    int retval = git_reference_foreach_glob(repo, glob, &name_callback, &ctx);
    free(glob);

    EM_RETURN_NIL_IF_NLE();
    if (retval == GIT_EUSER)
        return esym_nil;
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(reference_foreach_name, "REPO FUNC", "Call FUNC for every reference name in REPO.");
emacs_value egit_reference_foreach_name(emacs_env *env, emacs_value _repo, emacs_value func)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_FUNCTION(func);

    git_repository *repo = EGIT_EXTRACT(_repo);
    egit_generic_payload ctx = {.env = env, .func = func, .parent = NULL};
    int retval = git_reference_foreach_name(repo, &name_callback, &ctx);

    EM_RETURN_NIL_IF_NLE();
    if (retval == GIT_EUSER)
        return esym_nil;
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}
