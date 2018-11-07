#include "egit.h"

#ifndef EGIT_TREE_H
#define EGIT_TREE_H

EGIT_DEFUN(tree_lookup, emacs_value _repo, emacs_value _oid);

EGIT_DEFUN(tree_id, emacs_value _tree);
EGIT_DEFUN(tree_owner, emacs_value _tree);

#endif /* EGIT_TREE_H */
