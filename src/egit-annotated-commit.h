#include "egit.h"

#ifndef EGIT_ANNOTATED_COMMIT_H
#define EGIT_ANNOTATED_COMMIT_H

EGIT_DEFUN(annotated_commit_from_ref, emacs_value repo, emacs_value ref);

EGIT_DEFUN(annotated_commit_from_fetchhead,
           emacs_value repo,
           emacs_value branch_name,
           emacs_value remote_url,
           emacs_value id);

EGIT_DEFUN(annotated_commit_from_revspec,
           emacs_value repo,
           emacs_value revspec);

EGIT_DEFUN(annotated_commit_lookup,
           emacs_value repo,
           emacs_value id);

EGIT_DEFUN(annotated_commit_id,
           emacs_value ann);

#endif /* EGIT_ANNOTATED_COMMIT_H */
