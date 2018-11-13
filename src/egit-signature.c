#include "git2.h"

#include "egit.h"
#include "interface.h"


EGIT_DOC(signature_default, "REPO", "Create a new action signature with default user and now timestamp");
emacs_value egit_signature_default(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);

    git_repository *repo = EGIT_EXTRACT(_repo);

    git_signature *signature;
    int retval = git_signature_default(&signature, repo);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_SIGNATURE, signature, NULL);
}

EGIT_DOC(signature_name, "SIGNATURE", "Get the name from SIGNATURE.");
emacs_value egit_signature_name(emacs_env *env, emacs_value _sig)
{
    EGIT_ASSERT_SIGNATURE(_sig);
    git_signature *sig = EGIT_EXTRACT(_sig);
    return EM_STRING(sig->name);
}

EGIT_DOC(signature_email, "SIGNATURE", "Get the email from SIGNATURE.");
emacs_value egit_signature_email(emacs_env *env, emacs_value _sig)
{
    EGIT_ASSERT_SIGNATURE(_sig);
    git_signature *sig = EGIT_EXTRACT(_sig);
    return EM_STRING(sig->email);
}

EGIT_DOC(signature_time, "SIGNATURE", "Get the time from SIGNATURE.");
emacs_value egit_signature_time(emacs_env *env, emacs_value _sig)
{
    EGIT_ASSERT_SIGNATURE(_sig);
    git_signature *sig = EGIT_EXTRACT(_sig);
    return em_decode_time(env, sig->when.time, sig->when.offset * 60);
}
