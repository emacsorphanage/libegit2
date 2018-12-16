#ifndef EGIT_CHERRYPICK_H
#define EGIT_CHERRYPICK_H

#include "egit.h"

EGIT_DEFUN(cherrypick,
           emacs_value repo,
           emacs_value commit,
           emacs_value merge_opts,
           emacs_value checkout_opts,
           emacs_value mainline);

EGIT_DEFUN(cherrypick_commit,
           emacs_value repo,
           emacs_value cherrypick_commit,
           emacs_value our_commit,
           emacs_value merge_opts,
           emacs_value mainline);

#endif // EGIT_CHERRYPICK_H
