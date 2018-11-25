#include "git2.h"

#include "egit.h"
#include "interface.h"


EGIT_DOC(signature_default, "REPO", "Create a new action signature with default user and now timestamp.");
emacs_value egit_signature_default(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);

    git_repository *repo = EGIT_EXTRACT(_repo);

    git_signature *signature;
    int retval = git_signature_default(&signature, repo);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_SIGNATURE, signature, NULL);
}

EGIT_DOC(signature_now, "NAME EMAIL",
         "Create a new action signature with NAME and EMAIL and now timestamp.");
emacs_value egit_signature_now(emacs_env *env, emacs_value _name, emacs_value _email)
{
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_email);
    char *name = EM_EXTRACT_STRING(_name);
    char *email = EM_EXTRACT_STRING(_email);
    git_signature *signature;
    int retval = git_signature_now(&signature, name, email);
    free(name);
    free(email);
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
