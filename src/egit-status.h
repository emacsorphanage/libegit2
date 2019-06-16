#include "egit.h"

#ifndef EGIT_STATUS_H
#define EGIT_STATUS_H

EGIT_DEFUN(status_decode, emacs_value _status);
EGIT_DEFUN(status_file, emacs_value _repo, emacs_value _path);
EGIT_DEFUN(status_foreach_ext, emacs_value _repo, emacs_value function,
           emacs_value show, emacs_value flags, emacs_value pathspec,
           emacs_value baseline);
EGIT_DEFUN(status_should_ignore_p, emacs_value _repo, emacs_value _path);

#endif /* EGIT_STATUS_H */
