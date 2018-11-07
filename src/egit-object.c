#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


// =============================================================================
// Constructors

EGIT_DOC(object_lookup, "REPO OID &optional TYPE",
         "Look up an object in REPO by OID.\n"
         "If TYPE is given, error if the object has the wrong type:\n"
         "  - `blob'\n"
         "  - `commit'\n"
         "  - `tag'\n"
         "  - `tree'");
emacs_value egit_object_lookup(emacs_env *env, emacs_value _repo, emacs_value _oid, emacs_value _type)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    git_otype type;
    if (!EGIT_EXTRACT_BOOLEAN(_type))
        type = GIT_OBJ_ANY;
    else if (env->eq(env, _type, em_blob))
        type = GIT_OBJ_BLOB;
    else if (env->eq(env, _type, em_commit))
        type = GIT_OBJ_COMMIT;
    else if (env->eq(env, _type, em_tag))
        type = GIT_OBJ_TAG;
    else if (env->eq(env, _type, em_tree))
        type = GIT_OBJ_TREE;
    else {
        em_signal_wrong_value(env, _type);
        return em_nil;
    }

    git_object *object;
    int retval = git_object_lookup(&object, repo, &oid, type);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_OBJECT, object);
}

EGIT_DOC(object_lookup_prefix, "REPO OID",
         "Look up an object in REPO by shortened OID.\n"
         "If TYPE is given, error if the object has the wrong type:\n"
         "  - `blob'\n"
         "  - `commit'\n"
         "  - `tag'\n"
         "  - `tree'");
emacs_value egit_object_lookup_prefix(emacs_env *env, emacs_value _repo, emacs_value _oid, emacs_value _type)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    size_t len;
    EGIT_EXTRACT_OID_PREFIX(_oid, oid, len);

    git_otype type;
    if (!EGIT_EXTRACT_BOOLEAN(_type))
        type = GIT_OBJ_ANY;
    else if (env->eq(env, _type, em_blob))
        type = GIT_OBJ_BLOB;
    else if (env->eq(env, _type, em_commit))
        type = GIT_OBJ_COMMIT;
    else if (env->eq(env, _type, em_tag))
        type = GIT_OBJ_TAG;
    else if (env->eq(env, _type, em_tree))
        type = GIT_OBJ_TREE;
    else {
        em_signal_wrong_value(env, _type);
        return em_nil;
    }

    git_object *object;
    int retval = git_object_lookup_prefix(&object, repo, &oid, len, type);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_OBJECT, object);
}


// =============================================================================
// Getters

EGIT_DOC(object_id, "OBJ", "Return the ID for the given OBJ.");
emacs_value egit_object_id(emacs_env *env, emacs_value _obj)
{
    EGIT_ASSERT_OBJECT(_obj);
    git_object *obj = EGIT_EXTRACT(_obj);
    const git_oid *oid = git_object_id(obj);
    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}

EGIT_DOC(object_short_id, "OBJ", "Return the shortened ID for the given OBJ.");
emacs_value egit_object_short_id(emacs_env *env, emacs_value _obj)
{
    EGIT_ASSERT_OBJECT(_obj);
    git_object *obj = EGIT_EXTRACT(_obj);
    git_buf buf = {NULL, 0, 0};
    int retval = git_object_short_id(&buf, obj);
    EGIT_CHECK_ERROR(retval);
    EGIT_RET_BUF_AS_STRING(buf);
}
