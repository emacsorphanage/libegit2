#include "egit.h"

#ifndef EGIT_REPOSITORY_H
#define EGIT_REPOSITORY_H

EGIT_DEFUN(repository_init, emacs_value _path, emacs_value _is_bare);
EGIT_DEFUN(repository_open, emacs_value _path);
EGIT_DEFUN(repository_open_bare, emacs_value _path);

EGIT_DEFUN(repository_commondir, emacs_value _repo);
EGIT_DEFUN(repository_config, emacs_value _repo);
EGIT_DEFUN(repository_get_namespace, emacs_value _repo);
EGIT_DEFUN(repository_head, emacs_value _repo);
EGIT_DEFUN(repository_head_for_worktree, emacs_value _repo, emacs_value _name);
EGIT_DEFUN(repository_ident, emacs_value _repo);
EGIT_DEFUN(repository_index, emacs_value _repo);
EGIT_DEFUN(repository_message, emacs_value _repo);
EGIT_DEFUN(repository_path, emacs_value _repo);
EGIT_DEFUN(repository_state, emacs_value _repo);
EGIT_DEFUN(repository_workdir, emacs_value _repo);

EGIT_DEFUN(repository_detach_head, emacs_value _repo);
EGIT_DEFUN(repository_message_remove, emacs_value _repo);
EGIT_DEFUN(repository_set_head, emacs_value _repo, emacs_value _refname);
EGIT_DEFUN(repository_set_head_detached, emacs_value _repo, emacs_value _commitish);
EGIT_DEFUN(repository_set_ident, emacs_value _repo, emacs_value _name, emacs_value _email);
EGIT_DEFUN(repository_set_namespace, emacs_value _repo, emacs_value _nmspace);
EGIT_DEFUN(repository_set_workdir, emacs_value _repo, emacs_value _workdir, emacs_value _update_gitlink);
EGIT_DEFUN(repository_state_cleanup, emacs_value _repo);

EGIT_DEFUN(repository_bare_p, emacs_value _repo);
EGIT_DEFUN(repository_empty_p, emacs_value _repo);
EGIT_DEFUN(repository_shallow_p, emacs_value _repo);
EGIT_DEFUN(repository_worktree_p, emacs_value _repo);

EGIT_DEFUN(repository_discover, emacs_value _path, emacs_value _across_fs, emacs_value _ceiling_dirs);

#endif /* EGIT_REPOSITORY_H */
