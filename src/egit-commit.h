#include "egit.h"

#ifndef EGIT_COMMIT_H
#define EGIT_COMMIT_H

EGIT_DEFUN(commit_lookup, emacs_value _repo, emacs_value _oid);
EGIT_DEFUN(commit_lookup_prefix, emacs_value _repo, emacs_value _oid);

EGIT_DEFUN(commit_author, emacs_value _commit);
EGIT_DEFUN(commit_body, emacs_value _body);
EGIT_DEFUN(commit_committer, emacs_value _commit);
EGIT_DEFUN(commit_id, emacs_value _commit);
EGIT_DEFUN(commit_message, emacs_value _commit);
EGIT_DEFUN(commit_nth_gen_ancestor, emacs_value _commit, emacs_value _n);
EGIT_DEFUN(commit_owner, emacs_value _commit);
EGIT_DEFUN(commit_parent, emacs_value _commit, emacs_value _n);
EGIT_DEFUN(commit_parent_id, emacs_value _commit, emacs_value _n);
EGIT_DEFUN(commit_parentcount, emacs_value _commit);
EGIT_DEFUN(commit_summary, emacs_value _commit);
EGIT_DEFUN(commit_time, emacs_value _commit);
EGIT_DEFUN(commit_tree, emacs_value _commit);
EGIT_DEFUN(commit_tree_id, emacs_value _commit);

EGIT_DEFUN(commit_create, emacs_value _repo, emacs_value _refname, emacs_value _author,
           emacs_value _committer, emacs_value _msg, emacs_value _tree, emacs_value _parents);

#endif /* EGIT_COMMIT_H */
