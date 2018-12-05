#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
#include "interface.h"
#include "egit-tag.h"


// =============================================================================
// Constructors

EGIT_DOC(tag_lookup, "REPO OID", "Look up a tag in REPO by OID.");
emacs_value egit_tag_lookup(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    git_tag *tag;
    int retval = git_tag_lookup(&tag, repo, &oid);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_TAG, tag, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(tag_lookup_prefix, "REPO OID", "Lookup a tag in REPO by shortened OID.");
emacs_value egit_tag_lookup_prefix(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    size_t len;
    EGIT_EXTRACT_OID_PREFIX(_oid, oid, len);

    git_tag *tag;
    int retval = git_tag_lookup_prefix(&tag, repo, &oid, len);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_TAG, tag, EM_EXTRACT_USER_PTR(_repo));
}


// =============================================================================
// Foreach

int egit_tag_foreach_callback(const char *name, git_oid *oid, void *payload)
{
    egit_generic_payload *ctx = (egit_generic_payload*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[2];
    args[0] = EM_STRING(name);
    const char *oid_s = git_oid_tostr_s(oid);
    args[1] = EM_STRING(oid_s);

    env->funcall(env, ctx->func, 2, args);

    EM_RETURN_IF_NLE(GIT_EUSER);
    return 0;
}

EGIT_DOC(tag_foreach, "REPO FUNC",
         "Call FUNC on each tag in REPO.\n"
         "FUNC receives two arguments: the tag name and ID, both strings.");
emacs_value egit_tag_foreach(emacs_env *env, emacs_value _repo, emacs_value func)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_FUNCTION(func);

    egit_generic_payload ctx = {.env = env, .func = func, .parent = NULL};
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_tag_foreach(repo, &egit_tag_foreach_callback, &ctx);

    EM_RETURN_NIL_IF_NLE();
    if (retval == GIT_EUSER)
        return esym_nil;
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}


// =============================================================================
// Getters

EGIT_DOC(tag_id, "TAG", "Return the ID of TAG.");
emacs_value egit_tag_id(emacs_env *env, emacs_value _tag)
{
    EGIT_ASSERT_TAG(_tag);
    git_tag *tag = EGIT_EXTRACT(_tag);
    const git_oid *oid = git_tag_id(tag);
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(tag_owner, "TAG", "Return the repository that TAG belongs to.");
emacs_value egit_tag_owner(emacs_env *env, emacs_value _tag)
{
    EGIT_ASSERT_TAG(_tag);
    egit_object *owner = EGIT_EXTRACT_PARENT(_tag);
    owner->refcount++;
    return EM_USER_PTR(owner, egit_finalize);
}

EGIT_DOC(tag_message, "TAG", "Get the message of TAG, or nil.");
emacs_value egit_tag_message(emacs_env *env, emacs_value _tag)
{
    EGIT_ASSERT_TAG(_tag);
    git_tag *tag = EGIT_EXTRACT(_tag);
    const char *message = git_tag_message(tag);
    if (!message)
        return esym_nil;
    return EM_STRING(message);
}

EGIT_DOC(tag_name, "TAG", "Get the name of TAG.");
emacs_value egit_tag_name(emacs_env *env, emacs_value _tag)
{
    EGIT_ASSERT_TAG(_tag);
    git_tag *tag = EGIT_EXTRACT(_tag);
    const char *name = git_tag_name(tag);
    return EM_STRING(name);
}

EGIT_DOC(tag_peel, "TAG", "Recursively peel TAG until a non-tag object is found.");
emacs_value egit_tag_peel(emacs_env *env, emacs_value _tag)
{
    EGIT_ASSERT_TAG(_tag);
    git_tag *tag = EGIT_EXTRACT(_tag);
    git_object *obj;
    int retval = git_tag_peel(&obj, tag);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_OBJECT, obj, EGIT_EXTRACT_PARENT(_tag));
}

EGIT_DOC(tag_tagger, "TAG", "Get the tagger of TAG as a signature object.");
emacs_value egit_tag_tagger(emacs_env *env, emacs_value _tag)
{
    EGIT_ASSERT_TAG(_tag);
    git_tag *tag = EGIT_EXTRACT(_tag);
    const git_signature *sig = git_tag_tagger(tag);

    // Copy the signature so it can live independently from the commit
    git_signature *ret;
    int retval = git_signature_dup(&ret, sig);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_SIGNATURE, ret, NULL);
}

EGIT_DOC(tag_target, "TAG", "Get the object pointed to by TAG.");
emacs_value egit_tag_target(emacs_env *env, emacs_value _tag)
{
    EGIT_ASSERT_TAG(_tag);
    git_tag *tag = EGIT_EXTRACT(_tag);
    git_object *obj;
    int retval = git_tag_target(&obj, tag);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_OBJECT, obj, EGIT_EXTRACT_PARENT(_tag));
}

EGIT_DOC(tag_target_id, "TAG", "Get the object ID pointed to by TAG.");
emacs_value egit_tag_target_id(emacs_env *env, emacs_value _tag)
{
    EGIT_ASSERT_TAG(_tag);
    git_tag *tag = EGIT_EXTRACT(_tag);
    const git_oid *oid = git_tag_target_id(tag);
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(tag_target_type, "TAG", "Get the type of the object pointed to by TAG.");
emacs_value egit_tag_target_type(emacs_env *env, emacs_value _tag)
{
    EGIT_ASSERT_TAG(_tag);
    git_tag *tag = EGIT_EXTRACT(_tag);
    git_otype type = git_tag_target_type(tag);
    return em_findenum_otype(type);
}


// =============================================================================
// Miscellanoeus

EGIT_DOC(tag_list, "REPO &optional PATTERN",
         "Get a list of all tag names in REPO.\n"
         "If PATTERN is given, get only the matching tag names.");
emacs_value egit_tag_list(emacs_env *env, emacs_value _repo, emacs_value _pattern)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING_OR_NIL(_pattern);
    git_repository *repo = EGIT_EXTRACT(_repo);
    char *pattern = EM_EXTRACT_STRING_OR_NULL(_pattern);

    git_strarray out = {NULL, 0};
    int retval = pattern ? git_tag_list_match(&out, pattern, repo) : git_tag_list(&out, repo);
    free(pattern);
    EGIT_CHECK_ERROR(retval);

    EGIT_RET_STRARRAY(out);
}
