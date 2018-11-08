#include <stdarg.h>

#include "emacs-module.h"

#ifndef INTERFACE_H
#define INTERFACE_H

extern emacs_value em_nil, em_cons_p, em_integerp, em_functionp, em_stringp, em_t;

// Git object predicates and types
extern emacs_value em_libgit_object_p, em_libgit_repository_p, em_libgit_reference_p,
    em_libgit_signature_p, em_libgit_blame_p, em_libgit_commit_p, em_libgit_config_p,
    em_libgit_transaction_p, em_libgit_tree_p, em_libgit_index_p, em_libgit_index_entry_p,
    em_libgit_diff_p, em_libgit_diff_delta_p, em_libgit_diff_binary_p,
    em_libgit_diff_hunk_p, em_libgit_diff_line_p;
extern emacs_value em_repository, em_reference, em_commit, em_tree, em_blob, em_tag, em_object,
    em_signature, em_blame, em_transaction, em_config, em_index, em_index_entry, em_diff,
    em_diff_delta, em_diff_binary, em_diff_hunk, em_diff_line;

// Repository states
extern emacs_value em_merge, em_revert, em_revert_sequence, em_cherrypick,
    em_cherrypick_sequence, em_bisect, em_rebase, em_rebase_interactive, em_rebase_merge,
    em_apply_mailbox, em_apply_mailbox_or_rebase;

// Reference types
extern emacs_value em_direct, em_symbolic;

// File statuses
extern emacs_value em_index_new, em_index_modified, em_index_deleted,
    em_index_renamed, em_index_typechange, em_wt_new,
    em_wt_modified, em_wt_deleted, em_wt_typechange, em_wt_renamed,
    em_wt_unreadable, em_ignored, em_conflicted;

// Symbols for enum git_status_show_t
extern emacs_value em_index_only, em_workdir_only, em_index_and_workdir;

// Symbols for enum git_status_opt_t
extern emacs_value em_include_untracked, em_include_ignored, em_include_unmodified,
    em_exclude_submodules, em_recurse_untracked_dirs, em_disable_pathspec_match,
    em_recurse_ignored_dirs, em_renames_head_to_index, em_renames_index_to_workdir,
    em_sort_case_sensitively, em_sort_case_insensitively, em_renames_from_rewrites, em_no_refresh,
    em_update_index, em_include_unreadable, em_include_unreadable_as_untracked;

// Blame hunk properties
extern emacs_value em_lines_in_hunk,
  em_final_commit_id, em_final_start_line_number, em_final_signature,
  em_orig_commit_id, em_orig_path, em_orig_start_line_number, em_orig_signature,
  em_boundary;

// Blame options
extern emacs_value em_flags, em_min_match_characters, em_newest_commit,
  em_oldest_commit, em_min_line, em_max_line;

// Blame flags
extern emacs_value em_first_parent;

// Tree filemodes
extern emacs_value em_unreadable, em_blob_executable, em_link;

// Tree traversal
extern emacs_value em_pre, em_post, em_skip;

// Conflict markers
extern emacs_value em_base, em_ours, em_theirs;

// Index capabilities
extern emacs_value em_from_owner, em_no_symlinks, em_no_filemode, em_ignore_case;

// Symbols for diff option flags
extern emacs_value em_reverse, em_include_typechange, em_include_typechange_trees,
    em_ignore_filemode, em_ignore_submodules, em_include_casechange, em_skip_binary_check,
    em_enable_fast_untracked_dirs, em_force_text, em_force_binary, em_ignore_whitespace,
    em_ignore_whitespace_change, em_ignore_whitespace_eol, em_show_untracked_content,
    em_show_unmodified, em_patience, em_minimal, em_show_binary, em_indent_heuristic;

// Other diff options
extern emacs_value em_pathspec, em_notify, em_progress, em_context_lines, em_interhunk_lines,
    em_id_abbrev, em_max_size, em_old_prefix, em_new_prefix;

// Ignore submodules
extern emacs_value em_none, em_untracked, em_dirty, em_all;

// Diff callback symbols
extern emacs_value em_abort;

// Assert that VAL is a cons cell, signal an error and return otherwise.
#define EM_ASSERT_CONS(val)                                             \
    do { if (!em_assert(env, em_cons_p, (val))) return em_nil; } while (0)

// Assert that VAL is a function, signal an error and return otherwise.
#define EM_ASSERT_FUNCTION(val)                                         \
    do { if (!em_assert(env, em_functionp, (val))) return em_nil; } while (0)

// Assert that VAL is a string, signal an error and return otherwise.
#define EM_ASSERT_STRING(val)                                           \
    do { if (!em_assert(env, em_stringp, (val))) return em_nil; } while (0)

// Assert that VAL is a string or nil, signal an error and return otherwise.
#define EM_ASSERT_STRING_OR_NIL(val)                                    \
    do { if (EM_EXTRACT_BOOLEAN(val)) EM_ASSERT_STRING(val); } while (0)

// Assert that VAL is an integer, signal an error and return otherwise.
#define EM_ASSERT_INTEGER(val)                                          \
    do { if (!em_assert(env, em_integerp, (val))) return em_nil; } while (0)

// Assert that VAL is an integer or nil, signal an error and return otherwise.
#define EM_ASSERT_INTEGER_OR_NIL(val)                                   \
    do { if (EM_EXTRACT_BOOLEAN(val)) EM_ASSERT_INTEGER(val); } while (0)

// Normalize an emacs_value string path. This macro may return.
#define EM_NORMALIZE_PATH(val)                                  \
    do {                                                        \
        (val) = em_expand_file_name(env, val);                  \
        if (env->non_local_exit_check(env)) return em_nil;      \
    } while (0)

// Extract a boolean from an emacs_value.
#define EM_EXTRACT_BOOLEAN(val) (env->is_not_nil(env, (val)) ? 1 : 0)

// Extract a string from an emacs_value.
// Caller is reponsible for ensuring that the emacs_value represents a string.
#define EM_EXTRACT_STRING(val) em_get_string(env, (val));

// Extract an integer from an emacs_value.
// Caller is reponsible for ensuring that the emacs_value represents an integer.
#define EM_EXTRACT_INTEGER(val) env->extract_integer(env, (val))

// Extract an integer from an emacs_value with a default.
// Caller is reponsible for ensuring that the emacs_value represents an integer or nil.
#define EM_EXTRACT_INTEGER_OR_DEFAULT(val, default)                     \
    (EM_EXTRACT_BOOLEAN(val) ? EM_EXTRACT_INTEGER(val) : (default))

// Extract a string from an emacs_value, or NULL.
// Caller is reponsible for ensuring that the emacs_value represents a string or nil.
#define EM_EXTRACT_STRING_OR_NULL(val)                                  \
    (EM_EXTRACT_BOOLEAN(val) ? em_get_string(env, (val)) : NULL);

// Extract a user pointer from an emacs_value.
// Caller is responsible for ensuring that the emacs_value represents a user pointer.
#define EM_EXTRACT_USER_PTR(val) env->get_user_ptr(env, (val))

/**
 * Initiate a loop over an Emacs list.
 * If any element is not a cons cell or nil, it WILL signal an error and return nil.
 * @param var Variable bound to each car.
 * @param listvar List to loop over.
 * @param name Unique name identifying the loop.
 */
#define EM_DOLIST(var, listvar, name)                               \
    emacs_value __cell##name = (listvar);                           \
    __loop##name:                                                   \
    if (!EM_EXTRACT_BOOLEAN(__cell##name)) goto __end##name;        \
    if (!em_assert(env, em_cons_p, __cell##name)) return em_nil;    \
    emacs_value (var) = em_car(env, __cell##name)

/**
 * Close a loop over an Emacs lisp.
 * @param name: Unique name identifying the loop.
 */
#define EM_DOLIST_END(name)                     \
    __cell##name = em_cdr(env, __cell##name);   \
    goto __loop##name;                          \
    __end##name:

/**
 * Initialize the libegit2-emacs interface.
 * This function should only be called once.
 */
void em_init(emacs_env *env);

/**
 * Signal a wrong-type-argument error if PREDICATE does not apply to ARG.
 * @param env The active Emacs environment.
 * @param predicate The predicate.
 * @param arg The argument.
 * @return True iff an error was signaled.
 */
bool em_assert(emacs_env *env, emacs_value predicate, emacs_value arg);

/**
 * Signal an error originating form libgit2.
 * @param env The active Emacs environment.
 * @param _klass The error code.
 * @param _msg The error message.
 */
void em_signal_giterr(emacs_env *env, int _klass, const char* _msg);

/**
 * Signal a wrong-type-argument error.
 * @param env The active Emacs environment.
 * @param expected Symbol describing the expected type.
 * @param actual Emacs value that does not have the expected type.
 */
void em_signal_wrong_type(emacs_env *env, emacs_value expected, emacs_value actual);

/**
 * Signal a wrong-value-argument error.
 * @param env The active Emacs environment.
 * @param actual Emacs value that does not have the expected value.
 */
void em_signal_wrong_value(emacs_env *env, emacs_value actual);

/**
 * Signal an args-out-of-range error.
 * @param env The active Emacs environment.
 * @param index The erroneous index.
 */
void em_signal_args_out_of_range(emacs_env *env, intmax_t index);

/**
 * Return a string from an emacs_value.
 * Caller is responsible for ensuring that the value is a string, and to free the returned pointer.
 * @param env The active Emacs environment.
 * @param arg Emacs value representing a string.
 * @return The string (owned pointer).
 */
char *em_get_string(emacs_env *env, emacs_value arg);

/**
 * Call (cons car cdr) in Emacs.
 * @param env The active Emacs environment.
 * @param car The car.
 * @param cdr The cdr.
 * @return The cons cell.
 */
emacs_value em_cons(emacs_env *env, emacs_value car, emacs_value cdr);

/**
 * Call (consp cell) in Emacs.
 * @param env The active Emacs environment.
 * @param cell The cell you're testing.
 * @return True if cell is a cons cell, false otherwise.
 */
bool em_consp(emacs_env *env, emacs_value cell);

/**
 * Call (car cell) in Emacs.
 * @param env The active Emacs environment.
 * @param cell the cell to get the car of.
 * @return the car of the cell or nil.
 */
emacs_value em_car(emacs_env *env, emacs_value cell);

/**
 * Call (cdr cell) in Emacs.
 * @param env The active Emacs environment.
 * @param cell the cell to get the cdr of.
 * @return the cdr of the cell or nil.
 */
emacs_value em_cdr(emacs_env *env, emacs_value cell);

/**
 * Call (list OBJECTS...) in Emacs.
 * @param env The active Emacs environment.
 * @param objects Array of objects.
 * @param nobjects Number of \p objects.
 */
emacs_value em_list(emacs_env *env, emacs_value *objects, ptrdiff_t nobjects);

/**
 * Call (listp OBJECT) in Emacs.
 * @param env The active Emacs environment.
 * @param object An emacs value.
 */
bool em_listp(emacs_env *env, emacs_value object);

/**
 * Call (length SEQUENCE) in Emacs.
 * @param env The active Emacs environment.
 * @param object An emacs sequence.
 * @return Length of the sequence, or -1 on error.
 */
ptrdiff_t em_length(emacs_env *env, emacs_value sequence);

/**
 * Call (assq key list) in Emacs.
 * @param env The active Emacs environment.
 * @param key The key to lookup.
 * @param list The associated list (alist).
 * @return The first cons cell whose car is \p key.
 */
emacs_value em_assq(emacs_env *env, emacs_value key, emacs_value list);

/**
 * Call (define-error SYMBOL MSG) in Emacs.
 * @param env The active Emacs environment.
 * @param car The error symbol.
 * @param cdr The error description.
 */
void em_define_error(emacs_env *env, emacs_value symbol, const char *msg);

/**
 * Define a function in Emacs, using defalias.
 * @param env The active Emacs environment.
 * @param name Symbol name of the desired function.
 * @param func Function to bind.
 */
void em_defun(emacs_env *env, const char *name, emacs_value func);

/**
 * Call (expand-file-name PATH) in Emacs.
 * @param env The active Emacs environment.
 * @param path The path to expand.
 */
emacs_value em_expand_file_name(emacs_env *env, emacs_value path);

/**
 * Provide a feature to Emacs.
 * @param env The active Emacs environment.
 * @param name Symbol name of the feature to provide.
 */
void em_provide(emacs_env *env, const char *feature);

/**
 * Check if a value is a user pointer.
 * @param env The active Emacs environment.
 * @param val Value to check.
 * @return True iff val is a user pointer.
 */
bool em_user_ptrp(emacs_env *env, emacs_value val);

/**
 * Return the value of default-directory in Emacs.
 */
char *em_default_directory(emacs_env *env);

/**
 * Run (decode-time TIMESTAMP OFFSET) in Emacs.
 */
emacs_value em_decode_time(emacs_env *env, intmax_t timestamp, intmax_t offset);

#endif /* INTERFACE_H */
