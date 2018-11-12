#include "egit.h"

#ifndef EGIT_TAG_H
#define EGIT_TAG_H

EGIT_DEFUN(tag_lookup, emacs_value _repo, emacs_value _oid);
EGIT_DEFUN(tag_lookup_prefix, emacs_value _repo, emacs_value _oid);

EGIT_DEFUN(tag_foreach, emacs_value _repo, emacs_value func);

EGIT_DEFUN(tag_id, emacs_value _tag);
EGIT_DEFUN(tag_owner, emacs_value _tag);
EGIT_DEFUN(tag_message, emacs_value _tag);
EGIT_DEFUN(tag_name, emacs_value _tag);
EGIT_DEFUN(tag_peel, emacs_value _tag);
EGIT_DEFUN(tag_target, emacs_value _tag);
EGIT_DEFUN(tag_target_id, emacs_value _tag);
EGIT_DEFUN(tag_target_type, emacs_value _tag);

EGIT_DEFUN(tag_list, emacs_value _repo, emacs_value _pattern);

#endif /* EGIT_TAG_H */
