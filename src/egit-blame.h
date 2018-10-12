#include "egit.h"

#ifndef EGIT_BLAME_H
#define EGIT_BLAME_H

EGIT_DEFUN(blame_file,
           emacs_value _repo,
           emacs_value _path,
           emacs_value _options);

#endif /* EGIT_BLAME_H */
