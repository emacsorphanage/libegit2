#include "egit.h"

#ifndef EGIT_DEBUG_H
#define EGIT_DEBUG_H

EGIT_DEFUN(_refcount, emacs_value val);
EGIT_DEFUN(_wrapper, emacs_value val);
EGIT_DEFUN(_wrapped, emacs_value val);
EGIT_DEFUN(_parent_wrapper, emacs_value val);

#endif /* EGIT_DEBUG_H */
