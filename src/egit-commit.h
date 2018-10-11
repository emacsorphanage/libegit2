#include "egit.h"

#ifndef EGIT_COMMIT_H
#define EGIT_COMMIT_H

EGIT_DEFUN(libgit-commit-create, emacs_value _repo, emacs_value _update_ref, emacs_value _author, emacs_value _committer, emacs_value _message_encoding, emacs_value _message, emacs_value _tree, emacs_value _parent_count, emacs_value _parents);
