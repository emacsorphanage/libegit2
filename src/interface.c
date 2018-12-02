#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "emacs-module.h"
#include "interface.h"

#define GLOBREF(val) env->make_global_ref(env, (val))
#define INTERN(val) env->intern(env, (val))

// We store some globa references to emacs objects, mostly symbols,
// so that we don't have to waste time calling intern later on.

emacs_value em_nil, em_cons_p, em_integerp, em_functionp, em_stringp, em_t, em_symbol_value,
    em_list_p, em_user_ptr_p;

// Git object predicates and types
emacs_value em_libgit_object_p, em_libgit_repository_p, em_libgit_reference_p,
    em_libgit_signature_p, em_libgit_blame_p, em_libgit_commit_p, em_libgit_config_p,
    em_libgit_transaction_p, em_libgit_tree_p, em_libgit_index_p, em_libgit_index_entry_p,
    em_libgit_diff_p, em_libgit_diff_delta_p, em_libgit_diff_binary_p,
    em_libgit_diff_hunk_p, em_libgit_diff_line_p, em_libgit_tag_p, em_libgit_remote_p,
    em_libgit_refspec_p, em_libgit_blame_hunk_p, em_libgit_submodule_p, em_libgit_blob_p,
    em_libgit_cred_p, em_libgit_annotated_commit_p, em_libgit_reflog_p,
    em_libgit_reflog_entry_p, em_libgit_revwalk_p, em_libgit_treebuilder_p;
emacs_value em_repository, em_reference, em_commit, em_tree, em_blob, em_tag, em_object,
    em_signature, em_blame, em_config, em_transaction, em_index, em_index_entry, em_diff,
    em_diff_delta, em_diff_binary, em_diff_hunk, em_diff_line, em_remote, em_refspec,
    em_blame_hunk, em_submodule, em_cred, em_annotated_commit, em_reflog, em_reflog_entry,
    em_revwalk, em_treebuilder;

// Repository states
emacs_value em_merge, em_revert, em_revert_sequence, em_cherrypick,
    em_cherrypick_sequence, em_bisect, em_rebase, em_rebase_interactive, em_rebase_merge,
    em_apply_mailbox, em_apply_mailbox_or_rebase;

// Reference types
emacs_value em_direct, em_symbolic;

// File statuses
emacs_value em_index_new, em_index_modified, em_index_deleted,
    em_index_renamed, em_index_typechange, em_wt_new,
    em_wt_modified, em_wt_deleted, em_wt_typechange, em_wt_renamed,
    em_wt_unreadable, em_ignored, em_conflicted;

// Symbols for enum git_status_show_t
emacs_value em_index_only, em_workdir_only, em_index_and_workdir;

// Symbols for enum git_status_opt_t
emacs_value em_include_untracked, em_include_ignored, em_include_unmodified, em_exclude_submodules,
    em_recurse_untracked_dirs, em_disable_pathspec_match, em_recurse_ignored_dirs,
    em_renames_head_to_index, em_renames_index_to_workdir, em_sort_case_sensitively,
    em_sort_case_insensitively, em_renames_from_rewrites, em_no_refresh, em_update_index,
    em_include_unreadable, em_include_unreadable_as_untracked;

// Blame hunk properties
emacs_value em_lines_in_hunk,
  em_final_commit_id, em_final_start_line_number, em_final_signature,
  em_orig_commit_id, em_orig_path, em_orig_start_line_number, em_orig_signature,
  em_boundary;

// Blame options
emacs_value em_flags, em_min_match_characters, em_newest_commit,
  em_oldest_commit, em_min_line, em_max_line;

// Blame flags
emacs_value em_first_parent;

// Tree filemodes
emacs_value em_unreadable, em_blob_executable, em_link;

// Tree traversal
emacs_value em_pre, em_post, em_skip;

// Conflict markers
emacs_value em_base, em_ours, em_theirs;

// Index capabilities
emacs_value em_from_owner, em_no_symlinks, em_no_filemode, em_ignore_case;

// Symbols for diff option flags
emacs_value em_reverse, em_include_typechange, em_include_typechange_trees,
    em_ignore_filemode, em_ignore_submodules, em_include_casechange, em_skip_binary_check,
    em_enable_fast_untracked_dirs, em_force_text, em_force_binary, em_ignore_whitespace,
    em_ignore_whitespace_change, em_ignore_whitespace_eol, em_show_untracked_content,
    em_show_unmodified, em_patience, em_minimal, em_show_binary, em_indent_heuristic;

// Other diff options
emacs_value em_pathspec, em_notify, em_progress, em_context_lines, em_interhunk_lines,
    em_id_abbrev, em_max_size, em_old_prefix, em_new_prefix;

// Ignore submodules
emacs_value em_none, em_untracked, em_dirty, em_all;

// Diff callback symbols
emacs_value em_abort;

// Diff delta types
emacs_value em_unmodified, em_added, em_deleted, em_modified, em_renamed, em_copied,
    em_typechange, em_unreadable, em_conflicted;

// Diff sides
emacs_value em_old, em_new;

// Diff formats
emacs_value em_patch, em_patch_header, em_raw, em_name_only, em_name_status;

// Describe options
emacs_value em_max_candidates_tags, em_strategy, em_pattern,
    em_only_follow_first_parent, em_show_commit_oid_as_fallback, em_tags,
    em_abbreviated_size, em_always_use_long_format, em_dirty_suffix;

// Remote autotag
emacs_value em_auto;

// Remote directions
emacs_value em_fetch, em_push;

// Submodule status
emacs_value em_in_head, em_in_index, em_in_config, em_in_wd, em_index_added,
    em_wd_uninitialized, em_wd_added, em_wd_deleted, em_wd_modified,
    em_wd_index_modified, em_wd_wd_modified, em_wd_untracked;

// Checkout options
emacs_value em_safe, em_force, em_notify_when, em_conflict, em_updated,
    em_baseline;

// Index add
emacs_value em_check_pathspec;

// Push and fetch options
emacs_value em_callbacks, em_headers, em_proxy, em_type, em_auto,
    em_specified, em_url, em_credentials, em_certificate_check, em_prune, em_on,
    em_off, em_download_tags, em_update_fetchhead, em_sideband_progress,
    em_transfer_progress;

// Certificates
emacs_value em_x509, em_hostkey_libssh2, em_md5, em_sha1;

// Credentials
emacs_value em_userpass_plaintext, em_ssh_key, em_ssh_custom,
    em_ssh_interactive, em_username, em_ssh_memory, em_default;

// Libgit2 features
emacs_value em_threads, em_https, em_ssh;

// Merge options
emacs_value em_find_renames, em_fail_on_conflict, em_skip_reuc,
    em_no_recursive, em_rename_threshold, em_target_limit, em_recursion_limit,
    em_default_driver, em_file_favor, em_normal, em_union, em_file_flags,
    em_style_merge, em_style_diff3, em_simplify_alnum;

// Merge analysis
emacs_value em_up_to_date, em_fastforward, em_unborn, em_no_fastforward,
    em_fastforward_only;

// Revwalk sorts
emacs_value em_topological, em_time;

// Config file levels
emacs_value em_programdata, em_system, em_xdg, em_global, em_local,
    em_app;

// Reset types
emacs_value em_soft, em_mixed, em_hard;

// Fetch recurse submodules
emacs_value em_ondemand;

// Submodule update rules
emacs_value em_checkout;

// Libgit2 errors
emacs_value em_giterr, em_giterr_nomemory, em_giterr_os, em_giterr_invalid,
    em_giterr_reference, em_giterr_zlib, em_giterr_repository, em_giterr_config,
    em_giterr_regex, em_giterr_odb, em_giterr_index, em_giterr_object,
    em_giterr_net, em_giterr_tag, em_giterr_tree, em_giterr_indexer,
    em_giterr_ssl, em_giterr_submodule, em_giterr_thread, em_giterr_stash,
    em_giterr_checkout, em_giterr_fetchhead, em_giterr_merge, em_giterr_ssh,
    em_giterr_filter, em_giterr_revert, em_giterr_callback, em_giterr_cherrypick,
    em_giterr_describe, em_giterr_rebase, em_giterr_filesystem, em_giterr_patch,
    em_giterr_worktree, em_giterr_sha1;

// Symbols that are only reachable from within this file.
static emacs_value _cons, _defalias, _define_error, _expand_file_name,
    _not_implemented, _provide, _vector, _wrong_type_argument,
    _wrong_value_argument, _consp, _car, _cdr, _list, _length, _symbol_value,
    _default_directory, _assq, _args_out_of_range, _decode_time, _insert,
    _string_as_unibyte, _encode_time, _apply, _last;


void em_init(emacs_env *env)
{
    em_nil = GLOBREF(INTERN("nil"));
    em_cons_p = GLOBREF(INTERN("consp"));
    em_integerp = GLOBREF(INTERN("integerp"));
    em_functionp = GLOBREF(INTERN("functionp"));
    em_stringp = GLOBREF(INTERN("stringp"));
    em_t = GLOBREF(INTERN("t"));
    em_list_p = GLOBREF(INTERN("listp"));
    em_user_ptr_p = GLOBREF(INTERN("user-ptrp"));

    em_libgit_object_p = GLOBREF(INTERN("libgit-object-p"));
    em_libgit_repository_p = GLOBREF(INTERN("libgit-repository-p"));
    em_libgit_reference_p = GLOBREF(INTERN("libgit-reference-p"));
    em_libgit_signature_p = GLOBREF(INTERN("libgit-signature-p"));
    em_libgit_annotated_commit_p = GLOBREF(INTERN("libgit-annotated-commit-p"));
    em_libgit_blame_p = GLOBREF(INTERN("libgit-blame-p"));
    em_libgit_blame_hunk_p = GLOBREF(INTERN("libgit-blame-hunk-p"));
    em_libgit_commit_p = GLOBREF(INTERN("libgit-commit-p"));
    em_libgit_config_p = GLOBREF(INTERN("libgit-config-p"));
    em_libgit_transaction_p = GLOBREF(INTERN("libgit-transaction-p"));
    em_libgit_tree_p = GLOBREF(INTERN("libgit-tree-p"));
    em_libgit_index_p = GLOBREF(INTERN("libgit-index-p"));
    em_libgit_index_entry_p = GLOBREF(INTERN("libgit-index-entry-p"));
    em_libgit_diff_p = GLOBREF(INTERN("libgit-diff-p"));
    em_libgit_diff_delta_p = GLOBREF(INTERN("libgit-diff-delta-p"));
    em_libgit_diff_binary_p = GLOBREF(INTERN("libgit-diff-binary-p"));
    em_libgit_diff_hunk_p = GLOBREF(INTERN("libgit-diff-hunk-p"));
    em_libgit_diff_line_p = GLOBREF(INTERN("libgit-diff-line-p"));
    em_libgit_tag_p = GLOBREF(INTERN("libgit-tag-p"));
    em_libgit_remote_p = GLOBREF(INTERN("libgit-remote-p"));
    em_libgit_refspec_p = GLOBREF(INTERN("libgit-refspec-p"));
    em_libgit_submodule_p = GLOBREF(INTERN("libgit-submodule-p"));
    em_libgit_blob_p = GLOBREF(INTERN("libgit-blob-p"));
    em_libgit_cred_p = GLOBREF(INTERN("libgit-cred-p"));
    em_libgit_reflog_p = GLOBREF(INTERN("libgit-reflog-p"));
    em_libgit_reflog_entry_p = GLOBREF(INTERN("libgit-reflog-entry-p"));
    em_libgit_revwalk_p = GLOBREF(INTERN("libgit-revwalk-p"));
    em_libgit_treebuilder_p = GLOBREF(INTERN("libgit-treebuilder-p"));

    em_repository = GLOBREF(INTERN("repository"));
    em_reference = GLOBREF(INTERN("reference"));
    em_commit = GLOBREF(INTERN("commit"));
    em_tree = GLOBREF(INTERN("tree"));
    em_blob = GLOBREF(INTERN("blob"));
    em_tag = GLOBREF(INTERN("tag"));
    em_object = GLOBREF(INTERN("object"));
    em_signature = GLOBREF(INTERN("signature"));
    em_blame = GLOBREF(INTERN("blame"));
    em_blame_hunk = GLOBREF(INTERN("blame-hunk"));
    em_config = GLOBREF(INTERN("config"));
    em_transaction = GLOBREF(INTERN("transaction"));
    em_index = GLOBREF(INTERN("index"));
    em_index_entry = GLOBREF(INTERN("index-entry"));
    em_diff = GLOBREF(INTERN("diff"));
    em_diff_delta = GLOBREF(INTERN("diff-delta"));
    em_diff_binary = GLOBREF(INTERN("diff-binary"));
    em_diff_hunk = GLOBREF(INTERN("diff-hunk"));
    em_diff_line = GLOBREF(INTERN("diff-line"));
    em_remote = GLOBREF(INTERN("remote"));
    em_refspec = GLOBREF(INTERN("refspec"));
    em_submodule = GLOBREF(INTERN("submodule"));
    em_cred = GLOBREF(INTERN("cred"));
    em_annotated_commit = GLOBREF(INTERN("annotated-commit"));
    em_reflog = GLOBREF(INTERN("reflog"));
    em_reflog_entry = GLOBREF(INTERN("reflog-entry"));
    em_revwalk = GLOBREF(INTERN("revwalk"));
    em_treebuilder = GLOBREF(INTERN("treebuilder"));

    em_merge = GLOBREF(INTERN("merge"));
    em_revert = GLOBREF(INTERN("revert"));
    em_revert_sequence = GLOBREF(INTERN("revert-sequence"));
    em_cherrypick = GLOBREF(INTERN("cherrypick"));
    em_cherrypick_sequence = GLOBREF(INTERN("cherrypick-sequence"));
    em_bisect = GLOBREF(INTERN("bisect"));
    em_rebase = GLOBREF(INTERN("rebase"));
    em_rebase_interactive = GLOBREF(INTERN("rebase-interactive"));
    em_rebase_merge = GLOBREF(INTERN("rebase-merge"));
    em_apply_mailbox = GLOBREF(INTERN("apply-mailbox"));
    em_apply_mailbox_or_rebase = GLOBREF(INTERN("apply-mailbox-or-rebase"));

    em_direct = GLOBREF(INTERN("direct"));
    em_symbolic = GLOBREF(INTERN("symbolic"));

    em_index_new = GLOBREF(INTERN("index-new"));
    em_index_modified = GLOBREF(INTERN("index-modified"));
    em_index_deleted = GLOBREF(INTERN("index-deleted"));
    em_index_renamed = GLOBREF(INTERN("index-renamed"));
    em_index_typechange = GLOBREF(INTERN("index-typechange"));
    em_wt_new = GLOBREF(INTERN("wt-new"));
    em_wt_modified = GLOBREF(INTERN("wt-modified"));
    em_wt_deleted = GLOBREF(INTERN("wt-deleted"));
    em_wt_typechange = GLOBREF(INTERN("wt-typechange"));
    em_wt_renamed = GLOBREF(INTERN("wt-renamed"));
    em_wt_unreadable = GLOBREF(INTERN("wt-unreadable"));
    em_ignored = GLOBREF(INTERN("ignored"));
    em_conflicted = GLOBREF(INTERN("conflicted"));

    em_index_only = GLOBREF(INTERN("index-only"));
    em_workdir_only = GLOBREF(INTERN("workdir-only"));
    em_index_and_workdir = GLOBREF(INTERN("index-and-workdir"));

    em_include_untracked = GLOBREF(INTERN("include-untracked"));
    em_include_ignored = GLOBREF(INTERN("include-ignored"));
    em_include_unmodified = GLOBREF(INTERN("include-unmodified"));
    em_exclude_submodules = GLOBREF(INTERN("exclude-submodules"));
    em_recurse_untracked_dirs = GLOBREF(INTERN("recurse-untracked-dirs"));
    em_disable_pathspec_match = GLOBREF(INTERN("disable-pathspec-match"));
    em_recurse_ignored_dirs = GLOBREF(INTERN("recurse-ignored-dirs"));
    em_renames_head_to_index = GLOBREF(INTERN("renames-head-to-index"));
    em_renames_index_to_workdir = GLOBREF(INTERN("renames-index-to-workdir"));
    em_sort_case_sensitively = GLOBREF(INTERN("sort-case-sensitively"));
    em_sort_case_insensitively = GLOBREF(INTERN("sort-case-insensitively"));
    em_renames_from_rewrites = GLOBREF(INTERN("renames-from-rewrites"));
    em_no_refresh = GLOBREF(INTERN("no-refresh"));
    em_update_index = GLOBREF(INTERN("update-index"));
    em_include_unreadable = GLOBREF(INTERN("include-unreadable"));
    em_include_unreadable_as_untracked = GLOBREF(INTERN("include-unreadable-as-untracked"));

    em_lines_in_hunk = GLOBREF(INTERN("lines-in-hunk"));
    em_final_commit_id = GLOBREF(INTERN("final-commit-id"));
    em_final_start_line_number = GLOBREF(INTERN("final-start-line-number"));
    em_final_signature = GLOBREF(INTERN("final-signature"));
    em_orig_commit_id = GLOBREF(INTERN("orig-commit-id"));
    em_orig_path = GLOBREF(INTERN("orig-path"));
    em_orig_start_line_number = GLOBREF(INTERN("orig-start-line-number"));
    em_orig_signature = GLOBREF(INTERN("orig-signature"));
    em_boundary = GLOBREF(INTERN("boundary"));

    em_flags = GLOBREF(INTERN("flags"));
    em_min_match_characters = GLOBREF(INTERN("min-match-characters"));
    em_newest_commit = GLOBREF(INTERN("newest-commit"));
    em_oldest_commit = GLOBREF(INTERN("oldest-commit"));
    em_min_line = GLOBREF(INTERN("min-line"));
    em_max_line = GLOBREF(INTERN("max-line"));

    em_first_parent = GLOBREF(INTERN("first-parent"));

    em_unreadable = GLOBREF(INTERN("unreadable"));
    em_blob_executable = GLOBREF(INTERN("blob-executable"));
    em_link = GLOBREF(INTERN("link"));

    em_pre = GLOBREF(INTERN("pre"));
    em_post = GLOBREF(INTERN("post"));
    em_skip = GLOBREF(INTERN("skip"));

    em_base = GLOBREF(INTERN("base"));
    em_ours = GLOBREF(INTERN("ours"));
    em_theirs = GLOBREF(INTERN("theirs"));

    em_from_owner = GLOBREF(INTERN("from-owner"));
    em_no_symlinks = GLOBREF(INTERN("no-symlinks"));
    em_no_filemode = GLOBREF(INTERN("no-filemode"));
    em_ignore_case = GLOBREF(INTERN("ignore-case"));

    em_reverse = GLOBREF(INTERN("reverse"));
    em_include_typechange = GLOBREF(INTERN("include-typechange"));
    em_include_typechange_trees = GLOBREF(INTERN("include-typechange-trees"));
    em_ignore_filemode = GLOBREF(INTERN("ignore-filemode"));
    em_ignore_submodules = GLOBREF(INTERN("ignore-submodules"));
    em_include_casechange = GLOBREF(INTERN("include-casechange"));
    em_skip_binary_check = GLOBREF(INTERN("skip-binary-check"));
    em_enable_fast_untracked_dirs = GLOBREF(INTERN("enable-fast-untracked-dirs"));
    em_force_text = GLOBREF(INTERN("force-text"));
    em_force_binary = GLOBREF(INTERN("force-binary"));
    em_ignore_whitespace = GLOBREF(INTERN("ignore-whitespace"));
    em_ignore_whitespace_change = GLOBREF(INTERN("ignore-whitespace-change"));
    em_ignore_whitespace_eol = GLOBREF(INTERN("ignore-whitespace-eol"));
    em_show_untracked_content = GLOBREF(INTERN("show-untracked-content"));
    em_show_unmodified = GLOBREF(INTERN("show-unmodified"));
    em_patience = GLOBREF(INTERN("patience"));
    em_minimal = GLOBREF(INTERN("minimal"));
    em_show_binary = GLOBREF(INTERN("show-binary"));
    em_indent_heuristic = GLOBREF(INTERN("indent-heuristic"));

    em_pathspec = GLOBREF(INTERN("pathspec"));
    em_notify = GLOBREF(INTERN("notify"));
    em_progress = GLOBREF(INTERN("progress"));
    em_context_lines = GLOBREF(INTERN("context-lines"));
    em_interhunk_lines = GLOBREF(INTERN("interhunk-lines"));
    em_id_abbrev = GLOBREF(INTERN("id-abbrev"));
    em_max_size = GLOBREF(INTERN("max-size"));
    em_old_prefix = GLOBREF(INTERN("old-prefix"));
    em_new_prefix = GLOBREF(INTERN("new-prefix"));

    em_none = GLOBREF(INTERN("none"));
    em_untracked = GLOBREF(INTERN("untracked"));
    em_dirty = GLOBREF(INTERN("dirty"));
    em_all = GLOBREF(INTERN("all"));

    em_abort = GLOBREF(INTERN("abort"));

    em_unmodified = GLOBREF(INTERN("unmodified"));
    em_added = GLOBREF(INTERN("added"));
    em_deleted = GLOBREF(INTERN("deleted"));
    em_modified = GLOBREF(INTERN("modified"));
    em_renamed = GLOBREF(INTERN("renamed"));
    em_copied = GLOBREF(INTERN("copied"));
    em_typechange = GLOBREF(INTERN("typechange"));
    em_unreadable = GLOBREF(INTERN("unreadable"));
    em_conflicted = GLOBREF(INTERN("conflicted"));

    em_old = GLOBREF(INTERN("old"));
    em_new = GLOBREF(INTERN("new"));

    em_patch = GLOBREF(INTERN("patch"));
    em_patch_header = GLOBREF(INTERN("patch-header"));
    em_raw = GLOBREF(INTERN("raw"));
    em_name_only = GLOBREF(INTERN("name-only"));
    em_name_status = GLOBREF(INTERN("name-status"));

    em_max_candidates_tags = GLOBREF(INTERN("max-candidates-tags"));
    em_strategy = GLOBREF(INTERN("strategy"));
    em_pattern = GLOBREF(INTERN("pattern"));
    em_only_follow_first_parent = GLOBREF(INTERN("only-follow-first-parent"));
    em_show_commit_oid_as_fallback = GLOBREF(INTERN("show-commit-oid-as-fallback"));
    em_tags = GLOBREF(INTERN("tags"));
    em_abbreviated_size = GLOBREF(INTERN("abbreviated-size"));
    em_always_use_long_format = GLOBREF(INTERN("always-use-long-format"));
    em_dirty_suffix = GLOBREF(INTERN("dirty-suffix"));

    em_auto = GLOBREF(INTERN("auto"));

    em_fetch = GLOBREF(INTERN("fetch"));
    em_push = GLOBREF(INTERN("push"));

    em_in_head = GLOBREF(INTERN("in-head"));
    em_in_index = GLOBREF(INTERN("in-index"));
    em_in_config = GLOBREF(INTERN("in-config"));
    em_in_wd = GLOBREF(INTERN("in-wd"));
    em_index_added = GLOBREF(INTERN("index-added"));
    em_wd_uninitialized = GLOBREF(INTERN("wd-uninitialized"));
    em_wd_added = GLOBREF(INTERN("wd-added"));
    em_wd_deleted = GLOBREF(INTERN("wd-deleted"));
    em_wd_modified = GLOBREF(INTERN("wd-modified"));
    em_wd_index_modified = GLOBREF(INTERN("wd-index-modified"));
    em_wd_wd_modified = GLOBREF(INTERN("wd-wd-modified"));
    em_wd_untracked = GLOBREF(INTERN("wd-untracked"));

    em_check_pathspec = GLOBREF(INTERN("check-pathspec"));

    em_callbacks = GLOBREF(INTERN("callbacks"));
    em_headers = GLOBREF(INTERN("headers"));
    em_proxy = GLOBREF(INTERN("proxy"));
    em_type = GLOBREF(INTERN("type"));
    em_auto = GLOBREF(INTERN("auto"));
    em_specified = GLOBREF(INTERN("specified"));
    em_url = GLOBREF(INTERN("url"));
    em_sideband_progress = GLOBREF(INTERN("sideband-progress"));
    em_credentials = GLOBREF(INTERN("credentials"));
    em_certificate_check = GLOBREF(INTERN("certificate-check"));
    em_transfer_progress = GLOBREF(INTERN("transfer-progress"));
    em_prune = GLOBREF(INTERN("prune"));
    em_on = GLOBREF(INTERN("on"));
    em_off = GLOBREF(INTERN("off"));
    em_download_tags = GLOBREF(INTERN("download-tags"));
    em_update_fetchhead = GLOBREF(INTERN("update-fetchhead"));

    em_x509 = GLOBREF(INTERN("x509"));
    em_hostkey_libssh2 = GLOBREF(INTERN("hostkey-libssh2"));
    em_md5 = GLOBREF(INTERN("md5"));
    em_sha1 = GLOBREF(INTERN("sha1"));

    em_userpass_plaintext = GLOBREF(INTERN("userpass-plaintext"));
    em_ssh_key = GLOBREF(INTERN("ssh-key"));
    em_ssh_custom = GLOBREF(INTERN("ssh-custom"));
    em_ssh_interactive = GLOBREF(INTERN("ssh-interactive"));
    em_username = GLOBREF(INTERN("username"));
    em_ssh_memory = GLOBREF(INTERN("ssh-memory"));
    em_default = GLOBREF(INTERN("default"));

    em_find_renames = GLOBREF(INTERN("find-renames"));
    em_fail_on_conflict = GLOBREF(INTERN("fail-on-conflict"));
    em_skip_reuc = GLOBREF(INTERN("skip-reuc"));
    em_no_recursive = GLOBREF(INTERN("no-recursive"));
    em_rename_threshold = GLOBREF(INTERN("rename-threshold"));
    em_target_limit = GLOBREF(INTERN("target-limit"));
    em_recursion_limit = GLOBREF(INTERN("recursion-limit"));
    em_default_driver = GLOBREF(INTERN("default-driver"));
    em_file_favor = GLOBREF(INTERN("file-favor"));
    em_normal = GLOBREF(INTERN("normal"));
    em_union = GLOBREF(INTERN("union"));
    em_file_flags = GLOBREF(INTERN("file-flags"));
    em_style_merge = GLOBREF(INTERN("style-merge"));
    em_style_diff3 = GLOBREF(INTERN("style-diff3"));
    em_simplify_alnum = GLOBREF(INTERN("simplify-alnum"));

    em_up_to_date = GLOBREF(INTERN("up-to-date"));
    em_fastforward = GLOBREF(INTERN("fastforward"));
    em_unborn = GLOBREF(INTERN("unborn"));
    em_no_fastforward = GLOBREF(INTERN("no-fastforward"));
    em_fastforward_only = GLOBREF(INTERN("fastforward-only"));

    em_topological = GLOBREF(INTERN("topological"));
    em_time = GLOBREF(INTERN("time"));

    em_programdata = GLOBREF(INTERN("programdata"));
    em_system = GLOBREF(INTERN("system"));
    em_xdg = GLOBREF(INTERN("xdg"));
    em_global = GLOBREF(INTERN("global"));
    em_local = GLOBREF(INTERN("local"));
    em_app = GLOBREF(INTERN("app"));

    em_soft = GLOBREF(INTERN("soft"));
    em_mixed = GLOBREF(INTERN("mixed"));
    em_hard = GLOBREF(INTERN("hard"));

    em_ondemand = GLOBREF(INTERN("ondemand"));
    em_checkout = GLOBREF(INTERN("checkout"));

    em_threads = GLOBREF(INTERN("threads"));
    em_https = GLOBREF(INTERN("https"));
    em_ssh = GLOBREF(INTERN("ssh"));

    em_safe = GLOBREF(INTERN("safe"));
    em_force = GLOBREF(INTERN("force"));
    em_notify_when = GLOBREF(INTERN("notify-when"));
    em_conflict = GLOBREF(INTERN("conflict"));
    em_updated = GLOBREF(INTERN("updated"));
    em_baseline = GLOBREF(INTERN("baseline"));

    em_giterr = GLOBREF(INTERN("giterr"));
    em_giterr_nomemory = GLOBREF(INTERN("giterr-nomemory"));
    em_giterr_os = GLOBREF(INTERN("giterr-os"));
    em_giterr_invalid = GLOBREF(INTERN("giterr-invalid"));
    em_giterr_reference = GLOBREF(INTERN("giterr-reference"));
    em_giterr_zlib = GLOBREF(INTERN("giterr-zlib"));
    em_giterr_repository = GLOBREF(INTERN("giterr-repository"));
    em_giterr_config = GLOBREF(INTERN("giterr-config"));
    em_giterr_regex = GLOBREF(INTERN("giterr-regex"));
    em_giterr_odb = GLOBREF(INTERN("giterr-odb"));
    em_giterr_index = GLOBREF(INTERN("giterr-index"));
    em_giterr_object = GLOBREF(INTERN("giterr-object"));
    em_giterr_net = GLOBREF(INTERN("giterr-net"));
    em_giterr_tag = GLOBREF(INTERN("giterr-tag"));
    em_giterr_tree = GLOBREF(INTERN("giterr-tree"));
    em_giterr_indexer = GLOBREF(INTERN("giterr-indexer"));
    em_giterr_ssl = GLOBREF(INTERN("giterr-ssl"));
    em_giterr_submodule = GLOBREF(INTERN("giterr-submodule"));
    em_giterr_thread = GLOBREF(INTERN("giterr-thread"));
    em_giterr_stash = GLOBREF(INTERN("giterr-stash"));
    em_giterr_checkout = GLOBREF(INTERN("giterr-checkout"));
    em_giterr_fetchhead = GLOBREF(INTERN("giterr-fetchhead"));
    em_giterr_merge = GLOBREF(INTERN("giterr-merge"));
    em_giterr_ssh = GLOBREF(INTERN("giterr-ssh"));
    em_giterr_filter = GLOBREF(INTERN("giterr-filter"));
    em_giterr_revert = GLOBREF(INTERN("giterr-revert"));
    em_giterr_callback = GLOBREF(INTERN("giterr-callback"));
    em_giterr_cherrypick = GLOBREF(INTERN("giterr-cherrypick"));
    em_giterr_describe = GLOBREF(INTERN("giterr-describe"));
    em_giterr_rebase = GLOBREF(INTERN("giterr-rebase"));
    em_giterr_filesystem = GLOBREF(INTERN("giterr-filesystem"));
    em_giterr_patch = GLOBREF(INTERN("giterr-patch"));
    em_giterr_worktree = GLOBREF(INTERN("giterr-worktree"));
    em_giterr_sha1 = GLOBREF(INTERN("giterr-sha1"));

    _cons = GLOBREF(INTERN("cons"));
    _consp = GLOBREF(INTERN("consp"));
    _car = GLOBREF(INTERN("car"));
    _cdr = GLOBREF(INTERN("cdr"));
    _decode_time = GLOBREF(INTERN("decode-time"));
    _default_directory = GLOBREF(INTERN("default-directory"));
    _list = GLOBREF(INTERN("list"));
    _length = GLOBREF(INTERN("length"));
    _assq = GLOBREF(INTERN("assq"));
    _defalias = GLOBREF(INTERN("defalias"));
    _define_error = GLOBREF(INTERN("define-error"));
    _expand_file_name = GLOBREF(INTERN("expand-file-name"));
    _not_implemented = GLOBREF(INTERN("not-implemented"));
    _provide = GLOBREF(INTERN("provide"));
    _symbol_value = GLOBREF(INTERN("symbol-value"));
    _vector = GLOBREF(INTERN("vector"));
    _insert = GLOBREF(INTERN("insert"));
    _string_as_unibyte = GLOBREF(INTERN("string-as-unibyte"));
    _encode_time = GLOBREF(INTERN("encode-time"));
    _apply = GLOBREF(INTERN("apply"));
    _last = GLOBREF(INTERN("last"));

    _args_out_of_range = GLOBREF(INTERN("args-out-of-range"));
    _wrong_type_argument = GLOBREF(INTERN("wrong-type-argument"));
    _wrong_value_argument = GLOBREF(INTERN("wrong-value-argument"));

    em_define_error(env, _not_implemented, "Not implemented", em_nil);
    em_define_error(env, _wrong_value_argument, "Wrong argument value passed", em_nil);

    em_define_error(env, em_giterr, "Git error", em_nil);
    em_define_error(env, em_giterr_nomemory, "Git error: out of memory", em_giterr);
    em_define_error(env, em_giterr_os, "Git error: OS", em_giterr);
    em_define_error(env, em_giterr_invalid, "Git error: invalid", em_giterr);
    em_define_error(env, em_giterr_reference, "Git error: reference", em_giterr);
    em_define_error(env, em_giterr_zlib, "Git error: zlib", em_giterr);
    em_define_error(env, em_giterr_repository, "Git error: repository", em_giterr);
    em_define_error(env, em_giterr_config, "Git error: config", em_giterr);
    em_define_error(env, em_giterr_regex, "Git error: regex", em_giterr);
    em_define_error(env, em_giterr_odb, "Git error: ODB", em_giterr);
    em_define_error(env, em_giterr_index, "Git error: index", em_giterr);
    em_define_error(env, em_giterr_object, "Git error: object", em_giterr);
    em_define_error(env, em_giterr_net, "Git error: net", em_giterr);
    em_define_error(env, em_giterr_tag, "Git error: tag", em_giterr);
    em_define_error(env, em_giterr_tree, "Git error: tree", em_giterr);
    em_define_error(env, em_giterr_indexer, "Git error: indexer", em_giterr);
    em_define_error(env, em_giterr_ssl, "Git error: SSL", em_giterr);
    em_define_error(env, em_giterr_submodule, "Git error: submodule", em_giterr);
    em_define_error(env, em_giterr_thread, "Git error: thread", em_giterr);
    em_define_error(env, em_giterr_stash, "Git error: stash", em_giterr);
    em_define_error(env, em_giterr_checkout, "Git error: checkout", em_giterr);
    em_define_error(env, em_giterr_fetchhead, "Git error: fetch-head", em_giterr);
    em_define_error(env, em_giterr_merge, "Git error: merge", em_giterr);
    em_define_error(env, em_giterr_ssh, "Git error: SSH", em_giterr);
    em_define_error(env, em_giterr_filter, "Git error: filter", em_giterr);
    em_define_error(env, em_giterr_revert, "Git error: revert", em_giterr);
    em_define_error(env, em_giterr_callback, "Git error: callback", em_giterr);
    em_define_error(env, em_giterr_cherrypick, "Git error: cherry-pick", em_giterr);
    em_define_error(env, em_giterr_describe, "Git error: describe", em_giterr);
    em_define_error(env, em_giterr_rebase, "Git error: rebase", em_giterr);
    em_define_error(env, em_giterr_filesystem, "Git error: filesystem", em_giterr);
    em_define_error(env, em_giterr_patch, "Git error: patch", em_giterr);
    em_define_error(env, em_giterr_worktree, "Git error: worktree", em_giterr);
    em_define_error(env, em_giterr_sha1, "Git error: SHA-1", em_giterr);
}

/**
 * Call an Emacs function without error checking.
 * @param env The active Emacs environment.
 * @param func The function to call.
 * @param nargs The number of arguments that follow.
 * @return The function return value.
 */
static emacs_value em_funcall(emacs_env *env, emacs_value func, ptrdiff_t nargs, ...)
{
    emacs_value args[nargs];

    va_list vargs;
    va_start(vargs, nargs);
    for (ptrdiff_t i = 0; i < nargs; i++)
        args[i] = va_arg(vargs, emacs_value);
    va_end(vargs);

    return env->funcall(env, func, nargs, args);
}

bool em_assert(emacs_env *env, emacs_value predicate, emacs_value arg)
{
    bool cond = EM_EXTRACT_BOOLEAN(em_funcall(env, predicate, 1, arg));
    if (!cond)
        em_signal_wrong_type(env, predicate, arg);
    return cond;
}

ptrdiff_t em_assert_list(emacs_env *env, emacs_value predicate, emacs_value arg)
{
    ptrdiff_t nelems = 0;
    bool predp = EM_EXTRACT_BOOLEAN(predicate);

    while (em_consp(env, arg)) {
        emacs_value car = em_car(env, arg);
        if (predp && !em_assert(env, predicate, car))
            return -1;
        nelems++;
        arg = em_cdr(env, arg);
    }

    if (EM_EXTRACT_BOOLEAN(arg)) {
        em_signal_wrong_type(env, em_list_p, arg);
        return -1;
    }

    return nelems;
}

void em_signal(emacs_env *env, emacs_value error, const char *_msg)
{
    emacs_value msg = EM_STRING(_msg);
    env->non_local_exit_signal(env, error, em_cons(env, msg, em_nil));
}

void em_signal_wrong_type(emacs_env *env, emacs_value expected, emacs_value actual)
{
    env->non_local_exit_signal(
        env, _wrong_type_argument,
        em_cons(env, expected, em_cons(env, actual, em_nil))
    );
}

void em_signal_wrong_value(emacs_env *env, emacs_value actual)
{
    env->non_local_exit_signal(env, _wrong_value_argument, actual);
}

void em_signal_args_out_of_range(emacs_env *env, intmax_t index)
{
    env->non_local_exit_signal(env, _args_out_of_range, EM_INTEGER(index));
}

char *em_get_string_with_size(emacs_env *env, emacs_value arg, ptrdiff_t *size)
{
    env->copy_string_contents(env, arg, NULL, size);

    char *buf = (char*) malloc((*size) * sizeof(char));
    env->copy_string_contents(env, arg, buf, size);

    (*size)--;
    return buf;
}

char *em_get_string(emacs_env *env, emacs_value arg)
{
    ptrdiff_t size;
    return em_get_string_with_size(env, arg, &size);
}

emacs_value em_cons(emacs_env *env, emacs_value car, emacs_value cdr)
{
    return em_funcall(env, _cons, 2, car, cdr);
}

bool em_consp(emacs_env *env, emacs_value cell)
{
    return EM_EXTRACT_BOOLEAN(em_funcall(env, _consp, 1, cell));
}

emacs_value em_car(emacs_env *env, emacs_value cell)
{
    return em_funcall(env, _car, 1, cell);
}

emacs_value em_cdr(emacs_env *env, emacs_value cell)
{
    return em_funcall(env, _cdr, 1, cell);
}

emacs_value em_list(emacs_env *env, emacs_value *objects, ptrdiff_t nobjects)
{
    return env->funcall(env, _list, nobjects, objects);
}

bool em_listp(emacs_env *env, emacs_value object)
{
    return EM_EXTRACT_BOOLEAN(em_funcall(env, em_list_p, 1, object));
}

ptrdiff_t em_length(emacs_env *env, emacs_value sequence)
{
    emacs_value result = em_funcall(env, _length, 1, sequence);
    ptrdiff_t length = env->extract_integer(env, result);
    EM_RETURN_IF_NLE(-1);
    return length;
}

emacs_value em_assq(emacs_env *env, emacs_value key, emacs_value list)
{
  return em_funcall(env, _assq, 2, key, list);
}

void em_define_error(emacs_env *env, emacs_value symbol, const char *msg, emacs_value parent)
{
    em_funcall(env, _define_error, 3, symbol, EM_STRING(msg), parent);
}

void em_defun(emacs_env *env, const char *name, emacs_value func)
{
    em_funcall(env, _defalias, 2, INTERN(name), func);
}

emacs_value em_expand_file_name(emacs_env *env, emacs_value path)
{
    return em_funcall(env, _expand_file_name, 1, path);
}

void em_provide(emacs_env *env, const char *feature)
{
    em_funcall(env, _provide, 1, INTERN(feature));
}

bool em_user_ptrp(emacs_env *env, emacs_value val)
{
    return EM_EXTRACT_BOOLEAN(em_funcall(env, em_user_ptr_p, 1, val));
}

char *em_default_directory(emacs_env *env)
{
    emacs_value dir = em_funcall(env, _symbol_value, 1, _default_directory);
    dir = em_expand_file_name(env, dir);
    return em_get_string(env, dir);
}

emacs_value em_decode_time(emacs_env *env, intmax_t timestamp, intmax_t offset)
{
    return em_funcall(env, _decode_time, 2,
                      EM_INTEGER(timestamp),
                      EM_INTEGER(offset));
}

bool em_encode_time(emacs_env *env, emacs_value time, intmax_t *timestamp, intmax_t *offset)
{
    emacs_value encoded = em_funcall(env, _apply, 2, _encode_time, time);
    EM_RETURN_IF_NLE(false);

    emacs_value high = em_car(env, encoded);
    *timestamp = EM_EXTRACT_INTEGER(high) << 16;

    encoded = em_cdr(env, encoded);
    emacs_value low = em_car(env, encoded);
    *timestamp += EM_EXTRACT_INTEGER(low);

    emacs_value last = em_funcall(env, _last, 1, time);
    if (!em_consp(env, last)) {
        em_signal_wrong_type(env, last, em_cons_p);
        return false;
    }
    emacs_value zone = em_car(env, last);
    if (!EM_EXTRACT_BOOLEAN(em_funcall(env, em_integerp, 1, zone))) {
        em_signal_wrong_type(env, zone, em_integerp);
        return false;
    }
    *offset = EM_EXTRACT_INTEGER(zone);

    return true;
}

void em_insert(emacs_env *env, const char *ptr, size_t length)
{
    em_funcall(env, _insert, 1, env->make_string(env, ptr, length));
}

emacs_value em_string_as_unibyte(emacs_env *env, emacs_value str)
{
    return em_funcall(env, _string_as_unibyte, 1, str);
}
