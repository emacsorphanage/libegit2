#include "egit.h"

#ifndef EGIT_BLAME_H
#define EGIT_BLAME_H

EGIT_DEFUN(blame_file, emacs_value _repo, emacs_value _path, emacs_value _options);
EGIT_DEFUN(blame_get_hunk_byindex, emacs_value _blame, emacs_value _index);
EGIT_DEFUN(blame_get_hunk_byline, emacs_value _blame, emacs_value _line);
EGIT_DEFUN(blame_get_hunk_count, emacs_value _blame);

EGIT_DEFUN(blame_hunk_commit_id, emacs_value _hunk, emacs_value orig);
EGIT_DEFUN(blame_hunk_lines, emacs_value _hunk);
EGIT_DEFUN(blame_hunk_orig_path, emacs_value _hunk);
EGIT_DEFUN(blame_hunk_signature, emacs_value _hunk, emacs_value orig);
EGIT_DEFUN(blame_hunk_start_line_number, emacs_value _hunk, emacs_value orig);

#endif /* EGIT_BLAME_H */
