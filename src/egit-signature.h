#include "egit.h"

#ifndef EGIT_SIGNATURE_H
#define EGIT_SIGNATURE_H

EGIT_DEFUN(signature_default, emacs_value _repo);

EGIT_DEFUN(signature_name, emacs_value _sig);
EGIT_DEFUN(signature_email, emacs_value _sig);

#endif /* EGIT_SIGNATURE_H */
