#ifndef EGIT_INDEX_H
#define EGIT_INDEX_H

EGIT_DEFUN(index_caps, emacs_value _index);
EGIT_DEFUN(index_checksum, emacs_value _index);
EGIT_DEFUN(index_conflict_foreach, emacs_value _index, emacs_value function);
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

EGIT_DEFUN(index_conflicts_p, emacs_value _index);

EGIT_DEFUN(index_add_all, emacs_value _index, emacs_value _pathspec, emacs_value _options, emacs_value func);
EGIT_DEFUN(index_add_bypath, emacs_value _index, emacs_value _path);
EGIT_DEFUN(index_clear, emacs_value _index);
EGIT_DEFUN(index_read, emacs_value _index, emacs_value force);
EGIT_DEFUN(index_write, emacs_value _index);
EGIT_DEFUN(index_write_tree, emacs_value _index, emacs_value _repo);

#endif /* EGIT_INDEX_H */
