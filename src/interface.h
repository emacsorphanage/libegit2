#include <stdarg.h>
#include <string.h>

#include "emacs-module.h"

#ifndef INTERFACE_H
#define INTERFACE_H

extern emacs_value em_nil, em_cons_p, em_integerp, em_functionp, em_stringp, em_t,
    em_list_p, em_user_ptr_p;

// Git object predicates and types
extern emacs_value em_libgit_object_p, em_libgit_repository_p, em_libgit_reference_p,
    em_libgit_signature_p, em_libgit_blame_p, em_libgit_commit_p, em_libgit_config_p,
    em_libgit_transaction_p, em_libgit_tree_p, em_libgit_index_p, em_libgit_index_entry_p,
    em_libgit_diff_p, em_libgit_diff_delta_p, em_libgit_diff_binary_p,
    em_libgit_diff_hunk_p, em_libgit_diff_line_p, em_libgit_tag_p, em_libgit_remote_p,
    em_libgit_refspec_p, em_libgit_blame_hunk_p, em_libgit_submodule_p, em_libgit_blob_p,
    em_libgit_cred_p, em_libgit_annotated_commit_p, em_libgit_reflog_p,
    em_libgit_reflog_entry_p, em_libgit_revwalk_p, em_libgit_treebuilder_p;
extern emacs_value em_repository, em_reference, em_commit, em_tree, em_blob, em_tag, em_object,
    em_signature, em_blame, em_transaction, em_config, em_index, em_index_entry, em_diff,
    em_diff_delta, em_diff_binary, em_diff_hunk, em_diff_line, em_remote, em_refspec,
    em_blame_hunk, em_submodule, em_cred, em_annotated_commit, em_reflog, em_reflog_entry,
    em_revwalk, em_treebuilder;

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

// Diff delta types
extern emacs_value em_unmodified, em_added, em_deleted, em_modified, em_renamed, em_copied,
    em_typechange, em_unreadable, em_conflicted;

// Diff sides
extern emacs_value em_old, em_new;

// Diff formats
extern emacs_value em_patch, em_patch_header, em_raw, em_name_only, em_name_status;

// Describe options
extern emacs_value em_max_candidates_tags, em_strategy, em_pattern,
    em_only_follow_first_parent, em_show_commit_oid_as_fallback, em_tags,
    em_abbreviated_size, em_always_use_long_format, em_dirty_suffix;

// Remote autotag
extern emacs_value em_auto;

// Remote directions
extern emacs_value em_fetch, em_push;

// Submodule status
extern emacs_value em_in_head, em_in_index, em_in_config, em_in_wd, em_index_added,
    em_wd_uninitialized, em_wd_added, em_wd_deleted, em_wd_modified,
    em_wd_index_modified, em_wd_wd_modified, em_wd_untracked;

// Checkout options
extern emacs_value em_safe, em_force, em_notify_when, em_conflict, em_updated,
    em_baseline;

// Index add
extern emacs_value em_check_pathspec;

// Push and fetch options
extern emacs_value em_callbacks, em_headers, em_proxy, em_type, em_auto,
    em_specified, em_url, em_credentials, em_certificate_check, em_prune, em_on,
    em_off, em_download_tags, em_update_fetchhead, em_sideband_progress,
    em_transfer_progress;

// Certificates
extern emacs_value em_x509, em_hostkey_libssh2, em_md5, em_sha1;

// Credentials
extern emacs_value em_userpass_plaintext, em_ssh_key, em_ssh_custom,
    em_ssh_interactive, em_username, em_ssh_memory, em_default;

// Merge options
extern emacs_value em_find_renames, em_fail_on_conflict, em_skip_reuc,
    em_no_recursive, em_rename_threshold, em_target_limit, em_recursion_limit,
    em_default_driver, em_file_favor, em_normal, em_union, em_file_flags,
    em_style_merge, em_style_diff3, em_simplify_alnum;

// Merge analysis
extern emacs_value em_up_to_date, em_fastforward, em_unborn, em_no_fastforward,
    em_fastforward_only;

// Revwalk sorts
extern emacs_value em_topological, em_time;

// Config file levels
extern emacs_value em_programdata, em_system, em_xdg, em_global, em_local,
    em_app;

// Reset types
extern emacs_value em_soft, em_mixed, em_hard;

// Fetch recurse submodules
extern emacs_value em_ondemand;

// Submodule update rules
extern emacs_value em_checkout;

// Libgit2 features
extern emacs_value em_threads, em_https, em_ssh;

// Libgit2 errors
extern emacs_value em_giterr, em_giterr_nomemory, em_giterr_os, em_giterr_invalid,
    em_giterr_reference, em_giterr_zlib, em_giterr_repository, em_giterr_config,
    em_giterr_regex, em_giterr_odb, em_giterr_index, em_giterr_object,
    em_giterr_net, em_giterr_tag, em_giterr_tree, em_giterr_indexer,
    em_giterr_ssl, em_giterr_submodule, em_giterr_thread, em_giterr_stash,
    em_giterr_checkout, em_giterr_fetchhead, em_giterr_merge, em_giterr_ssh,
    em_giterr_filter, em_giterr_revert, em_giterr_callback, em_giterr_cherrypick,
    em_giterr_describe, em_giterr_rebase, em_giterr_filesystem, em_giterr_patch,
    em_giterr_worktree, em_giterr_sha1;

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

// Assert that VAL is an user-ptr, signal an error and return otherwise.
#define EM_ASSERT_USER_PTR(val)                                         \
    do { if (!em_assert(env, em_user_ptr_p, (val))) return em_nil; } while (0)

// Normalize an emacs_value string path. This macro may return.
#define EM_NORMALIZE_PATH(val)                                  \
    do {                                                        \
        (val) = em_expand_file_name(env, val);                  \
        EM_RETURN_NIL_IF_NLE();                                 \
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
    (EM_EXTRACT_BOOLEAN(val) ? em_get_string(env, (val)) : NULL)

// Extract a user pointer from an emacs_value.
// Caller is responsible for ensuring that the emacs_value represents a user pointer.
#define EM_EXTRACT_USER_PTR(val) env->get_user_ptr(env, (val))

// Call (eq a b) in Emacs
#define EM_EQ(a,b) (env->eq(env, (a), (b)))

// Create an Emacs integer
#define EM_INTEGER(val) (env->make_integer(env, (val)))

// Create an Emacs string from a null-terminated char*
#define EM_STRING(val) (env->make_string(env, (val), strlen(val)))

// Create an Emacs user pointer
#define EM_USER_PTR(val, fin) (env->make_user_ptr(env, (fin), (val)))

// Return if a non-local exit is set
#define EM_RETURN_IF_NLE(val)                    \
    do {                                         \
        if (env->non_local_exit_check(env))      \
            return (val);                        \
    } while (0)

#define EM_RETURN_NIL_IF_NLE() EM_RETURN_IF_NLE(em_nil)

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
 * Signal a wrong-type-argument error if ARG is not a proper list, every
 * element of which satisfies PREDICATE. PREDICATE may be nil, in which
 * case only the list-ness is checked.
 * @param env The active Emacs environment.
 * @param predicate The predicate.
 * @param arg The list.
 * @return The number of elements in the list, or negative if error.
 */
ptrdiff_t em_assert_list(emacs_env *env, emacs_value predicate, emacs_value arg);

/**
 * Signal an error with string message.
 * @param env The active Emacs environment.
 * @param error The error symbol.
 * @param _msg The error message.
 */
void em_signal(emacs_env *env, emacs_value error, const char* _msg);

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
 * Return a string with size from an emacs_value.
 * Caller is responsible for ensuring that the value is a string, and to free the returned pointer.
 * @param env The active Emacs environment.
 * @param arg Emacs value representing a string.
 * @param size Where the size will be stored.
 * @return The string (owned pointer).
 */
char *em_get_string_with_size(emacs_env *env, emacs_value arg, ptrdiff_t *size);

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
 * @param symbol The error symbol.
 * @param msg The error description.
 * @param parent The parent error.
 */
void em_define_error(emacs_env *env, emacs_value symbol, const char *msg, emacs_value parent);

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

/**
 * Run (encode-time ...) in Emacs.
 */
bool em_encode_time(emacs_env *env, emacs_value time, intmax_t *timestamp, intmax_t *offset);

/**
 * Run (insert ...) in Emacs.
 */
void em_insert(emacs_env *env, const char *ptr, size_t length);

/**
 * Convert an emacs string to unibyte.
 */
emacs_value em_string_as_unibyte(emacs_env *env, emacs_value str);

#endif /* INTERFACE_H */
