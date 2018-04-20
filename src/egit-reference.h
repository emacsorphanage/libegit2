#include "egit.h"

#ifndef EGIT_REFERENCE_H
#define EGIT_REFERENCE_H

EGIT_DEFUN(reference_name, emacs_value _ref);
EGIT_DEFUN(reference_owner, emacs_value _ref);
EGIT_DEFUN(reference_resolve, emacs_value _ref);
EGIT_DEFUN(reference_target, emacs_value _ref);

#endif /* EGIT_REFERENCE_H */
