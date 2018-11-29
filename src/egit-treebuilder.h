#include "egit.h"

#ifndef EGIT_TREEBUILDER_H
#define EGIT_TREEBUILDER_H

EGIT_DEFUN(treebuilder_new, emacs_value _repo, emacs_value _tree);

EGIT_DEFUN(treebuilder_entrycount, emacs_value _builder);

EGIT_DEFUN(treebuilder_clear, emacs_value _builder);

#endif /* EGIT_TREEBUILDER_H */
