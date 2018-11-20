#include "egit.h"

#ifndef EGIT_CHECKOUT_H
#define EGIT_CHECKOUT_H

EGIT_DEFUN(checkout_head, emacs_value _repo, emacs_value opts);
EGIT_DEFUN(checkout_index, emacs_value _repo, emacs_value _index, emacs_value opts);
EGIT_DEFUN(checkout_tree, emacs_value _repo, emacs_value _treeish, emacs_value opts);

#endif /* EGIT_CHECKOUT_H */
