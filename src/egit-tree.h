#include "egit.h"

#ifndef EGIT_TREE_H
#define EGIT_TREE_H

EGIT_DEFUN(tree_lookup, emacs_value _repo, emacs_value _oid);
EGIT_DEFUN(tree_lookup_prefix, emacs_value _repo, emacs_value _oid);

EGIT_DEFUN(tree_entry_byid, emacs_value _tree, emacs_value _oid);
EGIT_DEFUN(tree_entry_byindex, emacs_value _tree, emacs_value _index);
EGIT_DEFUN(tree_entry_byname, emacs_value _tree, emacs_value _name);
EGIT_DEFUN(tree_entry_bypath, emacs_value _tree, emacs_value _path);
EGIT_DEFUN(tree_entrycount, emacs_value _tree);
EGIT_DEFUN(tree_id, emacs_value _tree);
EGIT_DEFUN(tree_owner, emacs_value _tree);

EGIT_DEFUN(tree_walk, emacs_value _tree, emacs_value order, emacs_value function);

#endif /* EGIT_TREE_H */
