#ifndef EGIT_INDEX_H
#define EGIT_INDEX_H

EGIT_DEFUN(index_caps, emacs_value _index);
EGIT_DEFUN(index_checksum, emacs_value _index);
EGIT_DEFUN(index_conflict_get, emacs_value _index, emacs_value _path);
EGIT_DEFUN(index_entry_id, emacs_value _entry);
EGIT_DEFUN(index_entry_path, emacs_value _entry);
EGIT_DEFUN(index_entry_stage, emacs_value _entry);
EGIT_DEFUN(index_entrycount, emacs_value _index);
EGIT_DEFUN(index_get_byindex, emacs_value _index, emacs_value _n);
EGIT_DEFUN(index_get_bypath, emacs_value _index, emacs_value _path, emacs_value _stage);
EGIT_DEFUN(index_owner, emacs_value _index);
EGIT_DEFUN(index_path, emacs_value _index);
EGIT_DEFUN(index_version, emacs_value _index);

#endif /* EGIT_INDEX_H */
