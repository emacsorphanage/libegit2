#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "emacs-module.h"
#include "interface.h"

#define GLOBREF(val) env->make_global_ref(env, (val))
#define INTERN(val) env->intern(env, (val))

// We store some globa references to emacs objects, mostly symbols,
// so that we don't have to waste time calling intern later on.

emacs_value em_nil, em_cons_p, em_integerp, em_functionp, em_stringp, em_t, em_symbol_value;

// Git object predicates and types
emacs_value em_libgit_object_p, em_libgit_repository_p, em_libgit_reference_p,
    em_libgit_signature_p, em_libgit_blame_p, em_libgit_commit_p, em_libgit_config_p,
    em_libgit_transaction_p, em_libgit_tree_p, em_libgit_index_p, em_libgit_index_entry_p,
    em_libgit_diff_p, em_libgit_diff_delta_p, em_libgit_diff_binary_p,
    em_libgit_diff_hunk_p, em_libgit_diff_line_p, em_libgit_tag_p, em_libgit_remote_p,
    em_libgit_refspec_p, em_libgit_blame_hunk_p, em_libgit_submodule_p;
emacs_value em_repository, em_reference, em_commit, em_tree, em_blob, em_tag, em_object,
    em_signature, em_blame, em_config, em_transaction, em_index, em_index_entry, em_diff,
    em_diff_delta, em_diff_binary, em_diff_hunk, em_diff_line, em_remote, em_refspec,
    em_blame_hunk, em_submodule;

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

// Symbols that are only reachable from within this file.
static emacs_value _cons, _defalias, _define_error, _expand_file_name, _giterr,
    _not_implemented, _provide, _user_ptrp, _vector, _wrong_type_argument,
    _wrong_value_argument, _consp, _car, _cdr, _list, _listp, _length, _symbol_value,
    _default_directory, _assq, _args_out_of_range, _decode_time, _insert;


void em_init(emacs_env *env)
{
    em_nil = GLOBREF(INTERN("nil"));
    em_cons_p = GLOBREF(INTERN("consp"));
    em_integerp = GLOBREF(INTERN("integerp"));
    em_functionp = GLOBREF(INTERN("functionp"));
    em_stringp = GLOBREF(INTERN("stringp"));
    em_t = GLOBREF(INTERN("t"));

    em_libgit_object_p = GLOBREF(INTERN("libgit-object-p"));
    em_libgit_repository_p = GLOBREF(INTERN("libgit-repository-p"));
    em_libgit_reference_p = GLOBREF(INTERN("libgit-reference-p"));
    em_libgit_signature_p = GLOBREF(INTERN("libgit-signature-p"));
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

    _cons = GLOBREF(INTERN("cons"));
    _consp = GLOBREF(INTERN("consp"));
    _car = GLOBREF(INTERN("car"));
    _cdr = GLOBREF(INTERN("cdr"));
    _decode_time = GLOBREF(INTERN("decode-time"));
    _default_directory = GLOBREF(INTERN("default-directory"));
    _list = GLOBREF(INTERN("list"));
    _listp = GLOBREF(INTERN("listp"));
    _length = GLOBREF(INTERN("length"));
    _assq = GLOBREF(INTERN("assq"));
    _defalias = GLOBREF(INTERN("defalias"));
    _define_error = GLOBREF(INTERN("define-error"));
    _expand_file_name = GLOBREF(INTERN("expand-file-name"));
    _giterr = GLOBREF(INTERN("giterr"));
    _not_implemented = GLOBREF(INTERN("not-implemented"));
    _provide = GLOBREF(INTERN("provide"));
    _symbol_value = GLOBREF(INTERN("symbol-value"));
    _user_ptrp = GLOBREF(INTERN("user-ptrp"));
    _vector = GLOBREF(INTERN("vector"));
    _insert = GLOBREF(INTERN("insert"));

    _args_out_of_range = GLOBREF(INTERN("args-out-of-range"));
    _wrong_type_argument = GLOBREF(INTERN("wrong-type-argument"));
    _wrong_value_argument = GLOBREF(INTERN("wrong-value-argument"));

    em_define_error(env, _giterr, "Git error");
    em_define_error(env, _not_implemented, "Not implemented");
    em_define_error(env, _wrong_value_argument, "Wrong argument value passed");
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

void em_signal_giterr(emacs_env *env, int _klass, const char* _msg)
{
    emacs_value klass = EM_INTEGER(_klass);
    emacs_value msg = EM_STRING(_msg);
    env->non_local_exit_signal(env, _giterr, em_cons(env, klass, em_cons(env, msg, em_nil)));
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

char *em_get_string(emacs_env *env, emacs_value arg)
{
    ptrdiff_t size;
    env->copy_string_contents(env, arg, NULL, &size);

    char *buf = (char*) malloc(size * sizeof(char));
    env->copy_string_contents(env, arg, buf, &size);

    return buf;
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
    return EM_EXTRACT_BOOLEAN(em_funcall(env, _listp, 1, object));
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

void em_define_error(emacs_env *env, emacs_value symbol, const char *msg)
{
    em_funcall(env, _define_error, 2, symbol, EM_STRING(msg));
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
    return EM_EXTRACT_BOOLEAN(em_funcall(env, _user_ptrp, 1, val));
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

void em_insert(emacs_env *env, const char *ptr, size_t length)
{
    em_funcall(env, _insert, 1, env->make_string(env, ptr, length));
}
