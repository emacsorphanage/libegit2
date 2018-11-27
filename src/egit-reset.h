#include "egit.h"

#ifndef EGIT_RESET_H
#define EGIT_RESET_H

EGIT_DEFUN(reset,
           emacs_value repo,
           emacs_value target,
           emacs_value reset_type,
           emacs_value checkout_opts);

#endif /* EGIT_RESET_H */
