#include "egit.h"

#ifndef EGIT_MERGE_H
#define EGIT_MERGE_H

EGIT_DEFUN(merge, emacs_value _repo, emacs_value _heads, emacs_value _merge_opts, emacs_value _checkout_opts);
EGIT_DEFUN(merge_analysis, emacs_value _repo, emacs_value _heads);
EGIT_DEFUN(merge_base, emacs_value _repo, emacs_value _ids);
EGIT_DEFUN(merge_base_octopus, emacs_value _repo, emacs_value _ids);
EGIT_DEFUN(merge_bases, emacs_value _repo, emacs_value _ids);

#endif /* EGIT_MERGE_H */
