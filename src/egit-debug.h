#include "egit.h"

#ifndef EGIT_DEBUG_H
#define EGIT_DEBUG_H

void egit_signal_alloc(void *ptr);
void egit_signal_finalize(void *ptr);
void egit_signal_free(void *ptr);

EGIT_DEFUN_0(_allocs);
EGIT_DEFUN_0(_finalizes);
EGIT_DEFUN_0(_frees);
EGIT_DEFUN(_refcount, emacs_value val);
EGIT_DEFUN(_wrapper, emacs_value val);
EGIT_DEFUN(_wrapped, emacs_value val);
EGIT_DEFUN(_parent_wrapper, emacs_value val);

#endif /* EGIT_DEBUG_H */
