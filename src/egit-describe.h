#include "egit.h"

#ifndef EGIT_DESCRIBE_H
#define EGIT_DESCRIBE_H

EGIT_DEFUN(describe_commit, emacs_value _committish, emacs_value opts);
EGIT_DEFUN(describe_workdir, emacs_value _repo, emacs_value opts);

#endif /* EGIT_DESCRIBE_H */
