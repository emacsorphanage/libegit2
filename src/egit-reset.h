#include "egit.h"

#ifndef EGIT_RESET_H
#define EGIT_RESET_H

EGIT_DEFUN(reset,
           emacs_value repo,
           emacs_value target,
           emacs_value reset_type,
           emacs_value checkout_opts);

EGIT_DEFUN(reset_from_annotated,
           emacs_value repo,
           emacs_value ann,
           emacs_value reset_type,
           emacs_value checkout_opts);

EGIT_DEFUN(reset_default,
           emacs_value repo,
           emacs_value target,
           emacs_value pathspecs);

#endif /* EGIT_RESET_H */
