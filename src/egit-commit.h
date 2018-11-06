#include "egit.h"

#ifndef EGIT_COMMIT_H
#define EGIT_COMMIT_H

EGIT_DEFUN(commit_lookup, emacs_value _repo, emacs_value _oid);

EGIT_DEFUN(commit_id, emacs_value _commit);
EGIT_DEFUN(commit_owner, emacs_value _commit);
EGIT_DEFUN(commit_parent, emacs_value _commit, emacs_value _n);
EGIT_DEFUN(commit_parent_id, emacs_value _commit, emacs_value _n);
EGIT_DEFUN(commit_parentcount, emacs_value _commit);

#endif /* EGIT_COMMIT_H */
