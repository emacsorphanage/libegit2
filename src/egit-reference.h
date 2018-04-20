#ifndef EGIT_REFERENCE_H
#define EGIT_REFERENCE_H

emacs_value egit_reference_name(emacs_env *env, emacs_value _ref);
emacs_value egit_reference_owner(emacs_env *env, emacs_value _ref);
emacs_value egit_reference_resolve(emacs_env *env, emacs_value _ref);
emacs_value egit_reference_target(emacs_env *env, emacs_value _ref);

emacs_value egit_reference_p(emacs_env *env, emacs_value obj);

#endif /* EGIT_REFERENCE_H */
