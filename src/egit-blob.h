#ifndef EGIT_BLOB_H
#define EGIT_BLOB_H

EGIT_DEFUN(blob_lookup, emacs_value _repo, emacs_value _oid);
EGIT_DEFUN(blob_lookup_prefix, emacs_value _repo, emacs_value _oid);

EGIT_DEFUN(blob_id, emacs_value _blob);

#endif /* EGIT_BLOB_H */
