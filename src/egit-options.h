#include "emacs-module.h"

#ifndef EGIT_OPTIONS_H
#define EGIT_OPTIONS_H

emacs_value egit_checkout_options_parse(emacs_env *env, emacs_value alist, git_checkout_options *opts);
void egit_checkout_options_release(git_checkout_options *opts);

#endif /* EGIT_OPTIONS_H */
