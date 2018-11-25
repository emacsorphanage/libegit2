#include "egit.h"

#ifndef EGIT_GRAPH_H
#define EGIT_GRAPH_H

EGIT_DEFUN(graph_ahead_behind, emacs_value _repo, emacs_value _local, emacs_value _upstream);
EGIT_DEFUN(graph_descendant_p, emacs_value _repo, emacs_value _commit, emacs_value _ancestor);

#endif /* EGIT_GRAPH_H */
