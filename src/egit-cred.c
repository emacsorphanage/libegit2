#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-cred.h"



// =============================================================================
// Constructors

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

EGIT_DOC(cred_ssh_key_memory_new, "USERNAME PUBLIC-KEY PRIVATE-KEY PASSPHRASE",
         "Create an in-memory passphrase-protected SSH key object.");
emacs_value egit_cred_ssh_key_memory_new(
    emacs_env *env, emacs_value _username, emacs_value _public_key,
    emacs_value _private_key, emacs_value _passphrase)
{
    EM_ASSERT_STRING(_username);
    EM_ASSERT_STRING(_public_key);
    EM_ASSERT_STRING(_private_key);
    EM_ASSERT_STRING(_passphrase);

    char *username = EM_EXTRACT_STRING(_username);
    char *public_key = EM_EXTRACT_STRING(_public_key);
    char *private_key = EM_EXTRACT_STRING(_private_key);
    char *passphrase = EM_EXTRACT_STRING(_passphrase);

    git_cred *cred;
    int retval = git_cred_ssh_key_new(&cred, username, public_key, private_key, passphrase);
    free(username);
    free(public_key);
    free(private_key);
    free(passphrase);

    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CRED, cred, NULL);
}

EGIT_DOC(cred_ssh_key_new, "USERNAME PUBLIC-KEY PRIVATE-KEY PASSPHRASE",
         "Create a passphrase-protected SSH key object.\n"
         "PUBLIC-KEY and PRIVATE-KEY are paths to files on disk.");
emacs_value egit_cred_ssh_key_new(
    emacs_env *env, emacs_value _username, emacs_value _public_key,
    emacs_value _private_key, emacs_value _passphrase)
{
    EM_ASSERT_STRING(_username);
    EM_ASSERT_STRING(_public_key);
    EM_ASSERT_STRING(_private_key);
    EM_ASSERT_STRING(_passphrase);

    EM_NORMALIZE_PATH(_public_key);
    EM_NORMALIZE_PATH(_private_key);

    char *username = EM_EXTRACT_STRING(_username);
    char *public_key = EM_EXTRACT_STRING(_public_key);
    char *private_key = EM_EXTRACT_STRING(_private_key);
    char *passphrase = EM_EXTRACT_STRING(_passphrase);

    git_cred *cred;
    int retval = git_cred_ssh_key_new(&cred, username, public_key, private_key, passphrase);
    free(username);
    free(public_key);
    free(private_key);
    free(passphrase);

    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CRED, cred, NULL);
}

EGIT_DOC(cred_username_new, "USERNAME",
         "Create a new credential object that specifies USERNAME.\n"
         "This can be used in SSH authentication to query for the username if\n"
         "none are found in the URL.");
emacs_value egit_cred_username_new(emacs_env *env, emacs_value _username)
{
    EM_ASSERT_STRING(_username);
    char *username = EM_EXTRACT_STRING(_username);
    git_cred *cred;
    int retval = git_cred_username_new(&cred, username);
    free(username);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CRED, cred, NULL);
}

EGIT_DOC(cred_userpass_plaintext_new, "USERNAME PASSWORD",
         "Create a new plain-text credential object with USERNAME and PASSWORD.");
emacs_value egit_cred_userpass_plaintext_new(emacs_env *env, emacs_value _username, emacs_value _password)
{
    EM_ASSERT_STRING(_username);
    EM_ASSERT_STRING(_password);
    char *username = EM_EXTRACT_STRING(_username);
    char *password = EM_EXTRACT_STRING(_password);
    git_cred *cred;
    int retval = git_cred_userpass_plaintext_new(&cred, username, password);
    free(username);
    free(password);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_CRED, cred, NULL);
}


// =============================================================================
// Predicates

EGIT_DOC(cred_username_p, "CRED", "Return non-nil if CRED has username info.");
emacs_value egit_cred_username_p(emacs_env *env, emacs_value _cred)
{
    EGIT_ASSERT_CRED(_cred);
    git_cred *cred = EGIT_EXTRACT(_cred);
    return git_cred_has_username(cred) ? esym_t : esym_nil;
}
