#include "egit.h"

#ifndef EGIT_SUBMODULE_H
#define EGIT_SUBMODULE_H

EGIT_DEFUN(submodule_lookup, emacs_value _repo, emacs_value _name);

EGIT_DEFUN(submodule_branch, emacs_value _sub);
EGIT_DEFUN(submodule_head_id, emacs_value _sub);
EGIT_DEFUN(submodule_ignore, emacs_value _sub);
EGIT_DEFUN(submodule_index_id, emacs_value _sub);
EGIT_DEFUN(submodule_location, emacs_value _sub, emacs_value flag);
EGIT_DEFUN(submodule_name, emacs_value _sub);
EGIT_DEFUN(submodule_owner, emacs_value _sub);
EGIT_DEFUN(submodule_path, emacs_value _sub);
EGIT_DEFUN(submodule_status, emacs_value _repo, emacs_value _name, emacs_value _ignore, emacs_value flag);
EGIT_DEFUN(submodule_url, emacs_value _sub);
EGIT_DEFUN(submodule_wd_id, emacs_value _sub);

#endif /* EGIT_SUBMODULE_H */
