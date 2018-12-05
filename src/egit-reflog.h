#include "egit.h"

#ifndef EGIT_REFLOG_H
#define EGIT_REFLOG_H

EGIT_DEFUN(reflog_read, emacs_value _repo, emacs_value _refname);
EGIT_DEFUN(reflog_entry_byindex, emacs_value _reflog, emacs_value _index);
EGIT_DEFUN(reflog_entry_committer, emacs_value _entry);
EGIT_DEFUN(reflog_entry_id, emacs_value _entry, emacs_value _new);
EGIT_DEFUN(reflog_entry_message, emacs_value _entry);
EGIT_DEFUN(reflog_entrycount, emacs_value _reflog);
EGIT_DEFUN(reflog_append, emacs_value _reflog, emacs_value _id, emacs_value _committer, emacs_value _msg);
EGIT_DEFUN(reflog_drop, emacs_value _reflog, emacs_value _index, emacs_value rewrite);
EGIT_DEFUN(reflog_rename, emacs_value _reflog, emacs_value _old_refname, emacs_value _new_refname);
EGIT_DEFUN(reflog_write, emacs_value _reflog);

#endif /* EGIT_REFLOG_H */
