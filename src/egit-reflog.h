#include "egit.h"

#ifndef EGIT_REFLOG_H
#define EGIT_REFLOG_H

EGIT_DEFUN(reflog_read, emacs_value _repo, emacs_value _refname);
EGIT_DEFUN(reflog_entry_byindex, emacs_value _reflog, emacs_value _index);
EGIT_DEFUN(reflog_entrycount, emacs_value _reflog);

#endif /* EGIT_REFLOG_H */
