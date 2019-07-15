#include "egit.h"

#ifndef EGIT_DIFF_H
#define EGIT_DIFF_H

EGIT_DEFUN(diff_index_to_index, emacs_value _repo, emacs_value _old_index,
           emacs_value _new_index, emacs_value _opts);
EGIT_DEFUN(diff_index_to_workdir, emacs_value _repo, emacs_value _index,
           emacs_value _opts);
EGIT_DEFUN(diff_tree_to_index, emacs_value _repo, emacs_value _old_tree,
           emacs_value _index, emacs_value _opts);
EGIT_DEFUN(diff_tree_to_tree, emacs_value _repo, emacs_value _old_tree,
           emacs_value _new_tree, emacs_value _opts);
EGIT_DEFUN(diff_tree_to_workdir, emacs_value _repo, emacs_value _old_tree,
           emacs_value _opts);
EGIT_DEFUN(diff_tree_to_workdir_with_index, emacs_value _repo,
           emacs_value _old_tree, emacs_value _opts);

EGIT_DEFUN(diff_foreach, emacs_value _diff, emacs_value file_cb,
           emacs_value binary_cb, emacs_value hunk_cb, emacs_value line_cb);
EGIT_DEFUN(diff_print, emacs_value _diff, emacs_value _format, emacs_value _func);

EGIT_DEFUN(diff_delta_file_id, emacs_value _delta, emacs_value side);
EGIT_DEFUN(diff_delta_file_path, emacs_value _delta, emacs_value side);
EGIT_DEFUN(diff_delta_nfiles, emacs_value _delta);
EGIT_DEFUN(diff_delta_similarity, emacs_value _delta);
EGIT_DEFUN(diff_delta_status, emacs_value _delta);
EGIT_DEFUN(diff_delta_file_exists_p, emacs_value _delta, emacs_value side);

EGIT_DEFUN(diff_hunk_header, emacs_value _hunk);
EGIT_DEFUN(diff_hunk_lines, emacs_value _hunk, emacs_value side);
EGIT_DEFUN(diff_hunk_start, emacs_value _hunk, emacs_value side);

EGIT_DEFUN(diff_line_origin, emacs_value _line);
EGIT_DEFUN(diff_line_lineno, emacs_value _line, emacs_value side);
EGIT_DEFUN(diff_line_content, emacs_value _line);

EGIT_DEFUN(diff_get_delta, emacs_value _delta, emacs_value _index);
EGIT_DEFUN(diff_num_deltas, emacs_value _diff, emacs_value _type);

EGIT_DEFUN(diff_find_similar, emacs_value _diff, emacs_value _options);

#endif /* EGIT_DIFF_H */
