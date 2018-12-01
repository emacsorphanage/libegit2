#include "egit.h"

#ifndef EGIT_SUBMODULE_H
#define EGIT_SUBMODULE_H

EGIT_DEFUN(submodule_add_setup, emacs_value _repo, emacs_value _url, emacs_value _path, emacs_value linkp);
EGIT_DEFUN(submodule_lookup, emacs_value _repo, emacs_value _name);
EGIT_DEFUN(submodule_foreach, emacs_value _repo, emacs_value func);

EGIT_DEFUN(submodule_branch, emacs_value _sub);
EGIT_DEFUN(submodule_head_id, emacs_value _sub);
EGIT_DEFUN(submodule_ignore, emacs_value _sub);
EGIT_DEFUN(submodule_index_id, emacs_value _sub);
EGIT_DEFUN(submodule_location, emacs_value _sub, emacs_value flag);
EGIT_DEFUN(submodule_name, emacs_value _sub);
EGIT_DEFUN(submodule_open, emacs_value _sub);
EGIT_DEFUN(submodule_owner, emacs_value _sub);
EGIT_DEFUN(submodule_path, emacs_value _sub);
EGIT_DEFUN(submodule_status, emacs_value _repo, emacs_value _name, emacs_value _ignore, emacs_value flag);
EGIT_DEFUN(submodule_url, emacs_value _sub);
EGIT_DEFUN(submodule_wd_id, emacs_value _sub);

EGIT_DEFUN(submodule_reload, emacs_value _sub, emacs_value force);

#endif /* EGIT_SUBMODULE_H */
