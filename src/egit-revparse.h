#include "egit.h"

#ifndef EGIT_REVPARSE_H
#define EGIT_REVPARSE_H

EGIT_DEFUN(revparse, emacs_value _repo, emacs_value _spec);
EGIT_DEFUN(revparse_ext, emacs_value _repo, emacs_value _spec);
EGIT_DEFUN(revparse_single, emacs_value _repo, emacs_value _spec);

#endif /* EGIT_REVPARSE_H */
