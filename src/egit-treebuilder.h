#include "egit.h"

#ifndef EGIT_TREEBUILDER_H
#define EGIT_TREEBUILDER_H

EGIT_DEFUN(treebuilder_new, emacs_value _repo, emacs_value _tree);

EGIT_DEFUN(treebuilder_entrycount, emacs_value _builder);
EGIT_DEFUN(treebuilder_get, emacs_value _builder, emacs_value _path);

EGIT_DEFUN(treebuilder_clear, emacs_value _builder);
EGIT_DEFUN(treebuilder_insert, emacs_value _builder, emacs_value _path,
           emacs_value _oid, emacs_value _mode);
EGIT_DEFUN(treebuilder_remove, emacs_value _builder, emacs_value _path);
EGIT_DEFUN(treebuilder_write, emacs_value _builder);

EGIT_DEFUN(treebuilder_filter, emacs_value _builder, emacs_value func);

#endif /* EGIT_TREEBUILDER_H */
