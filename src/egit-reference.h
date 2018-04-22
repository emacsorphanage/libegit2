#include "egit.h"

#ifndef EGIT_REFERENCE_H
#define EGIT_REFERENCE_H

EGIT_DEFUN(reference_create, emacs_value _repo, emacs_value _name, emacs_value _id,
           emacs_value _force, emacs_value _log_message);
EGIT_DEFUN(reference_create_matching, emacs_value _repo, emacs_value _name, emacs_value _id,
           emacs_value _force, emacs_value _current_id, emacs_value _log_message);
EGIT_DEFUN(reference_dup, emacs_value _ref);
EGIT_DEFUN(reference_dwim, emacs_value _ref, emacs_value _shorthand);
EGIT_DEFUN(reference_lookup, emacs_value _repo, emacs_value _name);

EGIT_DEFUN(reference_delete, emacs_value _ref);
EGIT_DEFUN(reference_ensure_log, emacs_value _repo, emacs_value _refname);

EGIT_DEFUN(reference_branch_p, emacs_value _ref);
EGIT_DEFUN(reference_has_log_p, emacs_value _repo, emacs_value _refname);
EGIT_DEFUN(reference_name, emacs_value _ref);
EGIT_DEFUN(reference_note_p, emacs_value _ref);
EGIT_DEFUN(reference_owner, emacs_value _ref);
EGIT_DEFUN(reference_resolve, emacs_value _ref);
EGIT_DEFUN(reference_target, emacs_value _ref);

#endif /* EGIT_REFERENCE_H */
