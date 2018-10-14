#include "egit.h"

#ifndef EGIT_BLAME_H
#define EGIT_BLAME_H

EGIT_DEFUN(blame_file,
           emacs_value _repo,
           emacs_value _path,
           emacs_value _options);

EGIT_DEFUN(blame_get_hunk_byindex,
           emacs_value _blame,
           emacs_value _index);

EGIT_DEFUN(blame_get_hunk_byline,
           emacs_value _blame,
           emacs_value _line);

EGIT_DEFUN(blame_get_hunk_count,
           emacs_value _blame);

#endif /* EGIT_BLAME_H */
