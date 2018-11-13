#include "egit.h"

#ifndef EGIT_REFSPEC_H
#define EGIT_REFSPEC_H

EGIT_DEFUN(refspec_direction, emacs_value _refspec);
EGIT_DEFUN(refspec_dst, emacs_value _refspec);
EGIT_DEFUN(refspec_src, emacs_value _refspec);
EGIT_DEFUN(refspec_string, emacs_value _refspec);

EGIT_DEFUN(refspec_dst_matches_p, emacs_value _refspec, emacs_value _refname);
EGIT_DEFUN(refspec_force_p, emacs_value _refspec);
EGIT_DEFUN(refspec_src_matches_p, emacs_value _refspec, emacs_value _refname);

#endif /* EGIT_REFSPEC_H */
