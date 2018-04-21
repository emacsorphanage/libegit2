#include "egit.h"

#ifndef EGIT_REFERENCE_H
#define EGIT_REFERENCE_H

EGIT_DEFUN(reference_create, emacs_value _repo, emacs_value _name, emacs_value _id,
           emacs_value _force, emacs_value _log_message);
EGIT_DEFUN(reference_create_matching, emacs_value _repo, emacs_value _name, emacs_value _id,
           emacs_value _force, emacs_value _current_id, emacs_value _log_message);
EGIT_DEFUN(reference_lookup, emacs_value _repo, emacs_value _name);

EGIT_DEFUN(reference_delete, emacs_value _ref);

EGIT_DEFUN(reference_name, emacs_value _ref);
EGIT_DEFUN(reference_owner, emacs_value _ref);
EGIT_DEFUN(reference_resolve, emacs_value _ref);
EGIT_DEFUN(reference_target, emacs_value _ref);

#endif /* EGIT_REFERENCE_H */
