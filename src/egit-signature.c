#include <string.h>
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

EGIT_DOC(signature_from_string, "STRING",
         "Create a signature from a string representation on the form\n"
         "  Real Name <email> timestamp offset\n"
         "where timestamp is the number of seconds since the Unix epoch\n"
         "and offset is the timezone offset in +hhmm or -hhmm format.");
emacs_value egit_signature_from_string(emacs_env *env, emacs_value _str)
{
    EM_ASSERT_STRING(_str);
    char *string = EM_EXTRACT_STRING(_str);
    git_signature *signature;
    int retval = git_signature_from_buffer(&signature, string);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_SIGNATURE, signature, NULL);
}

EGIT_DOC(signature_new, "NAME EMAIL TIME",
         "Create a new signature with NAME, EMAIL and TIME timestamp.");
emacs_value egit_signature_new(emacs_env *env, emacs_value _name, emacs_value _email, emacs_value _time)
{
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_email);

    intmax_t timestamp, offset;
    if (!em_encode_time(env, _time, &timestamp, &offset))
        return esym_nil;

    char *name = EM_EXTRACT_STRING(_name);
    char *email = EM_EXTRACT_STRING(_email);
    git_signature *signature;
    int retval = git_signature_new(&signature, name, email, timestamp, offset / 60);
    free(name);
    free(email);

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
