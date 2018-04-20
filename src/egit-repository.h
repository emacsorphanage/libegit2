#include "egit.h"

#ifndef EGIT_REPOSITORY_H
#define EGIT_REPOSITORY_H

EGIT_DEFUN(clone, emacs_value _url, emacs_value _path);
EGIT_DEFUN(repository_init, emacs_value _path, emacs_value _is_bare);
EGIT_DEFUN(repository_open, emacs_value _path);
EGIT_DEFUN(repository_open_bare, emacs_value _path);

EGIT_DEFUN(repository_commondir, emacs_value _repo);
EGIT_DEFUN(repository_head, emacs_value _repo);
EGIT_DEFUN(repository_ident, emacs_value _repo);
EGIT_DEFUN(repository_path, emacs_value _repo);
EGIT_DEFUN(repository_state, emacs_value _repo);
EGIT_DEFUN(repository_workdir, emacs_value _repo);

EGIT_DEFUN(repository_p, emacs_value obj);
EGIT_DEFUN(repository_bare_p, emacs_value _repo);
EGIT_DEFUN(repository_empty_p, emacs_value _repo);
EGIT_DEFUN(repository_shallow_p, emacs_value _repo);
EGIT_DEFUN(repository_worktree_p, emacs_value _repo);

#endif /* EGIT_REPOSITORY_H */
