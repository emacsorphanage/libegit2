#include "egit.h"

#ifndef EGIT_PATHSPEC_H
#define EGIT_PATHSPEC_H

EGIT_DEFUN(pathspec_new, emacs_value _pathspecs);

EGIT_DEFUN(pathspec_matches_path,
	   emacs_value _pathspecs,
	   emacs_value _flags,
	   emacs_value _path);

#endif /* EGIT_REFSPEC_H */
