#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "emacs-module.h"
#include "interface.h"

#define GLOBREF(val) env->make_global_ref(env, (val))
#define INTERN(val) env->intern(env, (val))

// We store some globa references to emacs objects, mostly symbols,
// so that we don't have to waste time calling intern later on.

emacs_value em_nil, em_cons_p, em_integerp, em_stringp, em_t, em_symbol_value;

// Git object predicates
emacs_value em_libgit_object_p, em_libgit_repository_p, em_libgit_reference_p,
    em_libgit_signature_p, em_libgit_blame_p, em_libgit_commit_p;
emacs_value em_repository, em_reference, em_commit, em_tree, em_blob, em_tag, em_object,
    em_signature, em_blame;

// Repository states
emacs_value em_merge, em_revert, em_revert_sequence, em_cherrypick,
    em_cherrypick_sequence, em_bisect, em_rebase, em_rebase_interactive, em_rebase_merge,
    em_apply_mailbox, em_apply_mailbox_or_rebase;

// Reference types
emacs_value em_direct, em_symbolic;

// File statuses
emacs_value em_fs_index_new, em_fs_index_modified, em_fs_index_deleted,
    em_fs_index_renamed, em_fs_index_typechange, em_fs_wt_new,
    em_fs_wt_modified, em_fs_wt_deleted, em_fs_wt_typechange, em_fs_wt_renamed,
    em_fs_wt_unreadable, em_fs_ignored, em_fs_conflicted;

// Symbols for enum git_status_show_t
emacs_value em_status_show_index_only, em_status_show_workdir_only,
    em_status_show_index_and_workdir;

// Symbols for enum git_status_opt_t
emacs_value em_status_opt_include_untracked, em_status_opt_include_ignored,
    em_status_opt_include_unmodified, em_status_opt_exclude_submodules,
    em_status_opt_recurse_untracked_dirs, em_status_opt_disable_pathspec_match,
    em_status_opt_recurse_ignored_dirs, em_status_opt_renames_head_to_index,
    em_status_opt_renames_index_to_workdir, em_status_opt_sort_case_sensitively,
    em_status_opt_sort_case_insensitively, em_status_opt_renames_from_rewrites,
    em_status_opt_no_refresh, em_status_opt_update_index,
    em_status_opt_include_unreadable,
    em_status_opt_include_unreadable_as_untracked;

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

// Symbols that are only reachable from within this file.
static emacs_value _cons, _defalias, _define_error, _expand_file_name, _giterr,
    _not_implemented, _provide, _user_ptrp, _vector, _wrong_type_argument,
    _wrong_value_argument, _consp, _car, _cdr, _list, _listp, _length, _symbol_value,
    _default_directory, _assq;


void em_init(emacs_env *env)
{
    em_nil = GLOBREF(INTERN("nil"));
    em_cons_p = GLOBREF(INTERN("consp"));
    em_integerp = GLOBREF(INTERN("integerp"));
    em_stringp = GLOBREF(INTERN("stringp"));
    em_t = GLOBREF(INTERN("t"));

    em_libgit_object_p = GLOBREF(INTERN("libgit-object-p"));
    em_libgit_repository_p = GLOBREF(INTERN("libgit-repository-p"));
    em_libgit_reference_p = GLOBREF(INTERN("libgit-reference-p"));
    em_libgit_signature_p = GLOBREF(INTERN("libgit-signature-p"));
    em_libgit_blame_p = GLOBREF(INTERN("libgit-blame-p"));
    em_libgit_commit_p = GLOBREF(INTERN("libgit-commit-p"));

    em_repository = GLOBREF(INTERN("repository"));
    em_reference = GLOBREF(INTERN("reference"));
    em_commit = GLOBREF(INTERN("commit"));
    em_tree = GLOBREF(INTERN("tree"));
    em_blob = GLOBREF(INTERN("blob"));
    em_tag = GLOBREF(INTERN("tag"));
    em_object = GLOBREF(INTERN("object"));
    em_signature = GLOBREF(INTERN("signature"));
    em_blame = GLOBREF(INTERN("blame"));

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

    em_fs_index_new = GLOBREF(INTERN("index-new"));
    em_fs_index_modified = GLOBREF(INTERN("index-modified"));
    em_fs_index_deleted = GLOBREF(INTERN("index-deleted"));
    em_fs_index_renamed = GLOBREF(INTERN("index-renamed"));
    em_fs_index_typechange = GLOBREF(INTERN("index-typechange"));
    em_fs_wt_new = GLOBREF(INTERN("wt-new"));
    em_fs_wt_modified = GLOBREF(INTERN("wt-modified"));
    em_fs_wt_deleted = GLOBREF(INTERN("wt-deleted"));
    em_fs_wt_typechange = GLOBREF(INTERN("wt-typechange"));
    em_fs_wt_renamed = GLOBREF(INTERN("wt-renamed"));
    em_fs_wt_unreadable = GLOBREF(INTERN("wt-unreadable"));
    em_fs_ignored = GLOBREF(INTERN("ignored"));
    em_fs_conflicted = GLOBREF(INTERN("conflicted"));

    em_status_show_index_only = GLOBREF(INTERN("index-only"));
    em_status_show_workdir_only = GLOBREF(INTERN("workdir-only"));
    em_status_show_index_and_workdir = GLOBREF(INTERN("index-and-workdir"));

    em_status_opt_include_untracked = GLOBREF(INTERN("include-untracked"));
    em_status_opt_include_ignored = GLOBREF(INTERN("include-ignored"));
    em_status_opt_include_unmodified = GLOBREF(INTERN("include-unmodified"));
    em_status_opt_exclude_submodules = GLOBREF(INTERN("exclude-submodules"));
    em_status_opt_recurse_untracked_dirs =
        GLOBREF(INTERN("recurse-untracked-dirs"));
    em_status_opt_disable_pathspec_match =
        GLOBREF(INTERN("disable-pathspec-match"));
    em_status_opt_recurse_ignored_dirs =
        GLOBREF(INTERN("recurse-ignored-dirs"));
    em_status_opt_renames_head_to_index =
        GLOBREF(INTERN("renames-head-to-index"));
    em_status_opt_renames_index_to_workdir =
        GLOBREF(INTERN("renames-index-to-workdir"));
    em_status_opt_sort_case_sensitively =
        GLOBREF(INTERN("sort-case-sensitively"));
    em_status_opt_sort_case_insensitively =
        GLOBREF(INTERN("sort-case-insensitively"));
    em_status_opt_renames_from_rewrites =
        GLOBREF(INTERN("renames-from-rewrites"));
    em_status_opt_no_refresh = GLOBREF(INTERN("no-refresh"));
    em_status_opt_update_index = GLOBREF(INTERN("update-index"));
    em_status_opt_include_unreadable = GLOBREF(INTERN("include-unreadable"));
    em_status_opt_include_unreadable_as_untracked =
        GLOBREF(INTERN("include-unreadable-as-untracked"));

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

    _cons = GLOBREF(INTERN("cons"));
    _consp = GLOBREF(INTERN("consp"));
    _car = GLOBREF(INTERN("car"));
    _cdr = GLOBREF(INTERN("cdr"));
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
    bool cond = env->is_not_nil(env, em_funcall(env, predicate, 1, arg));
    if (!cond)
        em_signal_wrong_type(env, predicate, arg);
    return cond;
}

void em_signal_giterr(emacs_env *env, int _klass, const char* _msg)
{
    emacs_value klass = env->make_integer(env, _klass);
    emacs_value msg = env->make_string(env, _msg, strlen(_msg));
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
    return env->is_not_nil(env, em_funcall(env, _consp, 1, cell));
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
    return env->is_not_nil(env, em_funcall(env, _listp, 1, object));
}

ptrdiff_t em_length(emacs_env *env, emacs_value sequence)
{
    emacs_value result = em_funcall(env, _length, 1, sequence);
    ptrdiff_t length = env->extract_integer(env, result);
    if (env->non_local_exit_check(env)) {
        return -1;
    }
    return length;
}

emacs_value em_assq(emacs_env *env, emacs_value key, emacs_value list)
{
  return em_funcall(env, _assq, 2, key, list);
}

void em_define_error(emacs_env *env, emacs_value symbol, const char *msg)
{
    em_funcall(env, _define_error, 2, symbol, env->make_string(env, msg, strlen(msg)));
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
    return env->is_not_nil(env, em_funcall(env, _user_ptrp, 1, val));
}

char *em_default_directory(emacs_env *env)
{
    emacs_value dir = em_funcall(env, _symbol_value, 1, _default_directory);
    dir = em_expand_file_name(env, dir);
    return em_get_string(env, dir);
}
