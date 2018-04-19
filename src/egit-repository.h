#include "git2.h"

#ifndef EGIT_REPOSITORY_H
#define EGIT_REPOSITORY_H

emacs_value egit_clone(emacs_env *env, emacs_value _url, emacs_value _path);
emacs_value egit_repository_p(emacs_env *env, emacs_value obj);
emacs_value egit_repository_init(emacs_env *env, emacs_value _path, emacs_value _is_bare);
emacs_value egit_repository_open(emacs_env *env, emacs_value _path);

#endif /* EGIT_REPOSITORY_H */
