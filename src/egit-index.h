#ifndef EGIT_INDEX_H
#define EGIT_INDEX_H

EGIT_DEFUN(index_entry_id, emacs_value _entry);
EGIT_DEFUN(index_entry_path, emacs_value _entry);
EGIT_DEFUN(index_entry_stage, emacs_value _entry);
EGIT_DEFUN(index_entrycount, emacs_value _index);
EGIT_DEFUN(index_get_byindex, emacs_value _index, emacs_value _n);
EGIT_DEFUN(index_owner, emacs_value _index);

#endif /* EGIT_INDEX_H */
