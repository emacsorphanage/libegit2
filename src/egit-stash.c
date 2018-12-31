#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
#include "interface.h"
#include "egit-stash.h"


static int foreach_callback(size_t index, const char *msg, const git_oid *id, void *payload)
{
    egit_generic_payload *ctx = (egit_generic_payload*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[3];
    args[0] = EM_INTEGER(index);
    args[1] = EM_STRING(msg);

    const char *oid_s = git_oid_tostr_s(id);
    args[2] = EM_STRING(oid_s);

    env->funcall(env, ctx->func, 3, args);

    EM_RETURN_IF_NLE(GIT_EUSER);
    return 0;
}


EGIT_DOC(stash_drop, "REPO N", "Drop the Nth stash from REPO.");
emacs_value egit_stash_drop(emacs_env *env, emacs_value _repo, emacs_value _index)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_INTEGER(_index);

    git_repository *repo = EGIT_EXTRACT(_repo);
    size_t index = EM_EXTRACT_INTEGER(_index);

    int retval = git_stash_drop(repo, index);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}


EGIT_DOC(stash_foreach, "REPO FUNC",
         "Call FUNC for each stashed state in REPO.\n"
         "FUNC is called with three arguments: the stash index, message and commit ID.");
emacs_value egit_stash_foreach(emacs_env *env, emacs_value _repo, emacs_value func)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_FUNCTION(func);

    git_repository *repo = EGIT_EXTRACT(_repo);
    egit_generic_payload ctx = {.env = env, .func = func};

    int retval = git_stash_foreach(repo, foreach_callback, &ctx);

    EM_RETURN_NIL_IF_NLE();
    if (retval == GIT_EUSER)
        return esym_nil;
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}
