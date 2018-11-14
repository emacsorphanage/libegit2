#include "egit.h"

#ifndef EGIT_REVWALK_H
#define EGIT_REVWALK_H

EGIT_DEFUN(revwalk_new, emacs_value _repo);
EGIT_DEFUN(revwalk_repository, emacs_value _revwalk);

EGIT_DEFUN(revwalk_hide, emacs_value _revwalk, emacs_value _oid);
EGIT_DEFUN(revwalk_hide_glob, emacs_value _revwalk, emacs_value _glob);
EGIT_DEFUN(revwalk_hide_head, emacs_value _revwalk);
EGIT_DEFUN(revwalk_hide_ref, emacs_value _revwalk, emacs_value _refname);

EGIT_DEFUN(revwalk_push, emacs_value _revwalk, emacs_value _oid);
EGIT_DEFUN(revwalk_push_glob, emacs_value _revwalk, emacs_value _glob);
EGIT_DEFUN(revwalk_push_head, emacs_value _revwalk);
EGIT_DEFUN(revwalk_push_range, emacs_value _revwalk, emacs_value _range);
EGIT_DEFUN(revwalk_push_ref, emacs_value _revwalk, emacs_value _refname);

EGIT_DEFUN(revwalk_reset, emacs_value _revwalk);
EGIT_DEFUN(revwalk_simplify_first_parent, emacs_value _revwalk);
EGIT_DEFUN(revwalk_sorting, emacs_value _revwalk, emacs_value _mode);

EGIT_DEFUN(revwalk_foreach, emacs_value _revwalk, emacs_value _func, emacs_value _hide_pred);

#endif /* EGIT_REVWALK_H */
