#ifndef EGIT_REVERT_H
#define EGIT_REVERT_H

#include "egit.h"

EGIT_DEFUN(revert,
           emacs_value repo,
           emacs_value commit,
           emacs_value merge_opts,
           emacs_value checkout_opts,
           emacs_value mainline);

EGIT_DEFUN(revert_commit,
           emacs_value repo,
           emacs_value revert_commit,
           emacs_value our_commit,
           emacs_value merge_opts,
           emacs_value mainline);

#endif // EGIT_REVERT_H
