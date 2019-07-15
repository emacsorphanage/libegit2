#include "emacs-module.h"

#ifndef EGIT_OPTIONS_H
#define EGIT_OPTIONS_H

emacs_value egit_checkout_options_parse(emacs_env *env, emacs_value alist, git_checkout_options *opts);
void egit_checkout_options_release(git_checkout_options *opts);

emacs_value egit_merge_options_parse(emacs_env *env, emacs_value alist, git_merge_options *opts);

emacs_value egit_fetch_options_parse(emacs_env *env, emacs_value alist, git_fetch_options *opts);
void egit_fetch_options_release(git_fetch_options *opts);

emacs_value egit_push_options_parse(emacs_env *env, emacs_value alist, git_push_options *opts);
void egit_push_options_release(git_push_options *opts);

emacs_value egit_diff_find_options_parse(emacs_env *env, emacs_value alist, git_diff_find_options *opts);

#endif /* EGIT_OPTIONS_H */
