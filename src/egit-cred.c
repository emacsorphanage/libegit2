#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-cred.h"


EGIT_DOC(cred_default_new, "",
         "Create a default credential.\n"
         "Usable for Negotiate mechanisms like NTLM or Kerberos.");
emacs_value egit_cred_default_new(emacs_env *env)
{
    git_cred *cred;
    int retval = git_cred_default_new(&cred);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CRED, cred, NULL);
}

EGIT_DOC(cred_ssh_key_from_agent, "USERNAME",
         "Create a new SSH key credential for querying an SSH agent.");
emacs_value egit_cred_ssh_key_from_agent(emacs_env *env, emacs_value _username)
{
    EM_ASSERT_STRING(_username);
    char *username = EM_EXTRACT_STRING(_username);
    git_cred *cred;
    int retval = git_cred_ssh_key_from_agent(&cred, username);
    free(username);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CRED, cred, NULL);
}
