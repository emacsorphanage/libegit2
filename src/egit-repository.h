#include "git2.h"

#ifndef EGIT_REPOSITORY_H
#define EGIT_REPOSITORY_H

emacs_value egit_clone(emacs_env *env, emacs_value _url, emacs_value _path);
emacs_value egit_repository_init(emacs_env *env, emacs_value _path, emacs_value _is_bare);
emacs_value egit_repository_open(emacs_env *env, emacs_value _path);
emacs_value egit_repository_open_bare(emacs_env *env, emacs_value _path);

emacs_value egit_repository_path(emacs_env *env, emacs_value _repo);
emacs_value egit_repository_workdir(emacs_env *env, emacs_value _repo);

emacs_value egit_repository_p(emacs_env *env, emacs_value obj);
emacs_value egit_repository_bare_p(emacs_env *env, emacs_value _repo);
emacs_value egit_repository_empty_p(emacs_env *env, emacs_value _repo);
emacs_value egit_repository_shallow_p(emacs_env *env, emacs_value _repo);
emacs_value egit_repository_worktree_p(emacs_env *env, emacs_value _repo);

#endif /* EGIT_REPOSITORY_H */
