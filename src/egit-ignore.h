#include "egit.h"

#ifndef EGIT_IGNORE_H
#define EGIT_IGNORE_H

EGIT_DEFUN(add_rule, emacs_value _repo, emacs_value _rules);
EGIT_DEFUN(clear_internal_rules, emacs_value _repo);
EGIT_DEFUN(path_ignored_p, emacs_value _repo, emacs_value _path);

#endif /* EGIT_IGNORE_H */
