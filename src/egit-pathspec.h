#include "egit.h"

#ifndef EGIT_PATHSPEC_H
#define EGIT_PATHSPEC_H

EGIT_DEFUN(pathspec_new, emacs_value _pathspecs);

EGIT_DEFUN(pathspec_matches_path,
	   emacs_value _pathspecs,
	   emacs_value _flags,
	   emacs_value _path);

EGIT_DEFUN(pathspec_match_list_entrycount,
           emacs_value _match_list);

EGIT_DEFUN(pathspec_match_list_entry,
           emacs_value _match_list,
           emacs_value _pos);

EGIT_DEFUN(pathspec_match_list_diff_entry,
           emacs_value _match_list,
           emacs_value _pos);

EGIT_DEFUN(pathspec_match_list_failed_entrycount,
           emacs_value _match_list);

EGIT_DEFUN(pathspec_match_list_failed_entry,
           emacs_value _match_list,
           emacs_value _pos);

#endif /* EGIT_REFSPEC_H */
