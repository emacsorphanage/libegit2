#include "egit.h"

#ifndef EGIT_BRANCH_H
#define EGIT_BRANCH_H

EGIT_DEFUN(branch_create, emacs_value _repo, emacs_value _name, emacs_value _commitish, emacs_value _force);
EGIT_DEFUN(branch_create_from_annotated, emacs_value _repo, emacs_value _name, emacs_value _commitish, emacs_value _force);
EGIT_DEFUN(branch_lookup, emacs_value _repo, emacs_value _name, emacs_value _remote);
EGIT_DEFUN(branch_delete, emacs_value _ref);
EGIT_DEFUN(branch_checked_out_p, emacs_value _ref);
EGIT_DEFUN(branch_foreach, emacs_value _repo, emacs_value _type, emacs_value func);
EGIT_DEFUN(branch_head_p, emacs_value _ref);
EGIT_DEFUN(branch_move, emacs_value _ref, emacs_value _refname, emacs_value force);
EGIT_DEFUN(branch_name, emacs_value _ref);
EGIT_DEFUN(branch_remote_name, emacs_value _repo, emacs_value _branch);
EGIT_DEFUN(branch_set_upstream, emacs_value _ref, emacs_value _refname);
EGIT_DEFUN(branch_upstream, emacs_value _ref);
EGIT_DEFUN(branch_upstream_name, emacs_value _repo, emacs_value _refname);
EGIT_DEFUN(branch_upstream_remote, emacs_value _repo, emacs_value _refname);

#endif /* EGIT_BRANCH_H */
