#include "egit.h"

#ifndef EGIT_CRED_H
#define EGIT_CRED_H

EGIT_DEFUN_0(cred_default_new);
EGIT_DEFUN(cred_ssh_key_from_agent, emacs_value _username);
EGIT_DEFUN(cred_ssh_key_memory_new, emacs_value _username, emacs_value _public_key,
           emacs_value _private_key, emacs_value _passphrase);
EGIT_DEFUN(cred_ssh_key_new, emacs_value _username, emacs_value _public_key,
           emacs_value _private_key, emacs_value _passphrase);
EGIT_DEFUN(cred_username_new, emacs_value _username);
EGIT_DEFUN(cred_userpass_plaintext_new, emacs_value _username, emacs_value _password);

EGIT_DEFUN(cred_username_p, emacs_value _cred);

#endif /* EGIT_CRED_H */
