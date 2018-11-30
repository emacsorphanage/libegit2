#include "egit.h"

#ifndef EGIT_SIGNATURE_H
#define EGIT_SIGNATURE_H

EGIT_DEFUN(signature_default, emacs_value _repo);
EGIT_DEFUN(signature_from_string, emacs_value _str);
EGIT_DEFUN(signature_now, emacs_value _name, emacs_value _email);
EGIT_DEFUN(signature_new, emacs_value _name, emacs_value _email, emacs_value _time);

EGIT_DEFUN(signature_name, emacs_value _sig);
EGIT_DEFUN(signature_email, emacs_value _sig);
EGIT_DEFUN(signature_time, emacs_value _sig);

#endif /* EGIT_SIGNATURE_H */
