#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


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
