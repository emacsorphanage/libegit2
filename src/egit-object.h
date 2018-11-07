#include "egit.h"

#ifndef EGIT_OBJECT_H
#define EGIT_OBJECT_H

EGIT_DEFUN(object_lookup, emacs_value _repo, emacs_value _oid, emacs_value _type);
EGIT_DEFUN(object_lookup_prefix, emacs_value _repo, emacs_value _oid, emacs_value _type);

EGIT_DEFUN(object_id, emacs_value _obj);
EGIT_DEFUN(object_owner, emacs_value _obj);
EGIT_DEFUN(object_short_id, emacs_value _obj);

#endif /* EGIT_OBJECT_H */
