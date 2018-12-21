#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
#include "interface.h"
#include "egit-index.h"


// =============================================================================
// Getters

EGIT_DOC(index_caps, "INDEX",
         "Return a list of the capabilities of INDEX.\n"
         "This is a list of up to four symbols: `ignore-case',\n"
         "`no-filemode', `no-symlinks' and `from-owner'.");
emacs_value egit_index_caps(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    int caps = git_index_caps(index);
    return em_getlist_indexcap(env, caps);
}

EGIT_DOC(index_checksum, "INDEX", "Get the checksum of INDEX.");
emacs_value egit_index_checksum(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    const git_oid *oid = git_index_checksum(index);
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(index_conflict_foreach, "INDEX FUNCTION",
         "Call FUNCTION for each conflict in INDEX.\n"
         "FUNCTION is called with four arguments: the path to the\n"
         "file under conflict, and the three index entries associated\n"
         "with the base version, our version and their version.\n\n"
         "The return value of FUNCTION is ignored. INDEX must not\n"
         "be modified as a side-effect.");
emacs_value egit_index_conflict_foreach(emacs_env *env, emacs_value _index, emacs_value function)
{
    EGIT_ASSERT_INDEX(_index);
    EM_ASSERT_FUNCTION(function);
    git_index *index = EGIT_EXTRACT(_index);
    git_index_conflict_iterator *iter;
    int retval = git_index_conflict_iterator_new(&iter, index);
    EGIT_CHECK_ERROR(retval);

    egit_object *index_wrp = EM_EXTRACT_USER_PTR(_index);
    const git_index_entry *base, *ours, *theirs;

    while (true) {
        int retval = git_index_conflict_next(&base, &ours, &theirs, iter);
        if (retval != 0) {
            git_index_conflict_iterator_free(iter);
            if (retval == GIT_ITEROVER)
                return esym_nil;
            EGIT_CHECK_ERROR(retval);
            return esym_nil;  // Should be unreachable
        }

        emacs_value args[4];
        args[0] = EM_STRING(base->path);
        args[1] = egit_wrap(env, EGIT_INDEX_ENTRY, base, index_wrp);
        args[2] = egit_wrap(env, EGIT_INDEX_ENTRY, ours, index_wrp);
        args[3] = egit_wrap(env, EGIT_INDEX_ENTRY, theirs, index_wrp);
        env->funcall(env, function, 4, args);

        if (env->non_local_exit_check(env)) {
            git_index_conflict_iterator_free(iter);
            return esym_nil;
        }
    }
}

EGIT_DOC(index_conflict_get, "INDEX PATH",
         "Get the INDEX entries associated with a conflict at PATH.\n"
         "Returns a list with three elements: the base entry, our and their side.");
emacs_value egit_index_conflict_get(emacs_env *env, emacs_value _index, emacs_value _path)
{
    EGIT_ASSERT_INDEX(_index);
    EM_ASSERT_STRING(_path);
    git_index *index = EGIT_EXTRACT(_index);
    char *path = EM_EXTRACT_STRING(_path);
    const git_index_entry *base, *ours, *theirs;
    int retval = git_index_conflict_get(&base, &ours, &theirs, index, path);
    free(path);
    EGIT_CHECK_ERROR(retval);

    emacs_value ret[3];
    egit_object *index_wrp = EM_EXTRACT_USER_PTR(_index);
    ret[0] = egit_wrap(env, EGIT_INDEX_ENTRY, base, index_wrp);
    ret[1] = egit_wrap(env, EGIT_INDEX_ENTRY, ours, index_wrp);
    ret[2] = egit_wrap(env, EGIT_INDEX_ENTRY, theirs, index_wrp);
    return em_list(env, ret, 3);
}

EGIT_DOC(index_entry_id, "ENTRY", "Get the path of the given index ENTRY.");
emacs_value egit_index_entry_id(emacs_env *env, emacs_value _entry)
{
    EGIT_ASSERT_INDEX_ENTRY(_entry);
    git_index_entry *entry = EGIT_EXTRACT(_entry);
    const char *oid_s = git_oid_tostr_s(&entry->id);
    return EM_STRING(oid_s);
}

EGIT_DOC(index_entry_path, "ENTRY", "Get the path of the given index ENTRY.");
emacs_value egit_index_entry_path(emacs_env *env, emacs_value _entry)
{
    EGIT_ASSERT_INDEX_ENTRY(_entry);
    git_index_entry *entry = EGIT_EXTRACT(_entry);
    return EM_STRING(entry->path);
}

EGIT_DOC(index_entry_stage, "ENTRY",
         "Get the stage of ENTRY.\n"
         "This is either nil (indicating not a conflict), or one of the symbols\n"
         "`base', `ours', or `theirs'.");
emacs_value egit_index_entry_stage(emacs_env *env, emacs_value _entry)
{
    EGIT_ASSERT_INDEX_ENTRY(_entry);
    git_index_entry *entry = EGIT_EXTRACT(_entry);
    int stage = git_index_entry_stage(entry);
    return em_findenum_stage(stage);
}

EGIT_DOC(index_entrycount, "INDEX", "Get the number of entries in INDEX.");
emacs_value egit_index_entrycount(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    size_t count = git_index_entrycount(index);
    return EM_INTEGER(count);
}

EGIT_DOC(index_get_byindex, "INDEX N", "Get the Nth entry in INDEX.");
emacs_value egit_index_get_byindex(emacs_env *env, emacs_value _index, emacs_value _n)
{
    EGIT_ASSERT_INDEX(_index);
    EM_ASSERT_INTEGER(_n);
    git_index *index = EGIT_EXTRACT(_index);
    intmax_t n = EM_EXTRACT_INTEGER(_n);
    const git_index_entry *entry = git_index_get_byindex(index, n);
    if (!entry) {
        em_signal_args_out_of_range(env, n);
        return esym_nil;
    }
    return egit_wrap(env, EGIT_INDEX_ENTRY, entry, EM_EXTRACT_USER_PTR(_index));
}

EGIT_DOC(index_get_bypath, "INDEX PATH &optional STAGE",
         "Get an entry from INDEX by PATH.\n"
         "STAGE may be one of the symbols `base', `ours', `theirs',\n"
         "in which case look for an entry at the given stage.\n"
         "If STAGE is nil, look for a conflict-less entry.");
emacs_value egit_index_get_bypath(emacs_env *env, emacs_value _index, emacs_value _path, emacs_value _stage)
{
    EGIT_ASSERT_INDEX(_index);
    EM_ASSERT_STRING(_path);

    int stage;
    if (!em_findsym_stage(&stage, env, _stage, true))
        return esym_nil;

    git_index *index = EGIT_EXTRACT(_index);
    char *path = EM_EXTRACT_STRING(_path);
    const git_index_entry *entry = git_index_get_bypath(index, path, stage);
    free(path);

    if (!entry)
        return esym_nil; // TODO: Better to signal an error?
    return egit_wrap(env, EGIT_INDEX_ENTRY, entry, EM_EXTRACT_USER_PTR(_index));
}

EGIT_DOC(index_owner, "INDEX", "Return the repository associated with INDEX.");
emacs_value egit_index_owner(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    egit_object *owner = EGIT_EXTRACT_PARENT(_index);

    // Bare indexes may not have an owner
    if (!owner)
        return esym_nil;

    owner->refcount++;
    return EM_USER_PTR(owner, egit_finalize);
}

EGIT_DOC(index_path, "INDEX", "Get the path to the index file on disk.");
emacs_value egit_index_path(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    const char *path = git_index_path(index);
    if (!path)
        return esym_nil;
    return EM_STRING(path);
}

EGIT_DOC(index_version, "INDEX", "Return the version of INDEX.");
emacs_value egit_index_version(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    unsigned int version = git_index_version(index);
    return EM_INTEGER(version);
}


// =============================================================================
// Predicates

EGIT_DOC(index_conflicts_p, "INDEX", "Return non-nil if INDEX has conflicts.");
emacs_value egit_index_conflicts_p(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    return git_index_has_conflicts(index) ? esym_t : esym_nil;
}


// =============================================================================
// Operations

static int add_all_callback(const char *path, const char *matched_pathspec, void *payload)
{
    egit_generic_payload *ctx = (egit_generic_payload*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[2];
    args[0] = EM_STRING(path);
    args[1] = matched_pathspec ? EM_STRING(matched_pathspec) : esym_nil;
    emacs_value retval = env->funcall(env, ctx->func, 2, args);

    EM_RETURN_IF_NLE(GIT_EUSER);
    if (EM_EQ(retval, esym_abort))
        return GIT_EUSER;
    if (EM_EQ(retval, esym_skip))
        return 1;
    return 0;
}

EGIT_DOC(index_add_all, "INDEX &optional PATHSPEC OPTIONS FUNC",
         "Add all unstaged changes to INDEX.\n"
         "PATHSPEC is list of path patterns to consider.\n"
         "OPTIONS is a list containing any of the symbols:\n"
         "- `force': stage files that are ignored,\n"
         "- `disable-pathspec-match': treat PATHSPEC as literal\n"
         "     paths instead of patterns,\n"
         "- `check-pathspec`: check that each entry in PATHSPEC\n"
         "     that matches a filename exactly is either not\n"
         "     ignored or already in the index.\n\n"
         "FUNC is a function that is called with two arguments for\n"
         "each added file: the path to the file, and the matching\n"
         "pathspec, if any.");
emacs_value egit_index_add_all(
    emacs_env *env, emacs_value _index, emacs_value _pathspec,
    emacs_value _opts, emacs_value func)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);

    git_index_add_option_t options = GIT_INDEX_ADD_DEFAULT;
    if (!em_setflags_list(&options, env, _opts, true, em_setflag_index_add_option))
        return esym_nil;

    git_index_matched_path_cb callback = NULL;
    egit_generic_payload payload = {.env = env, .func = func, .parent = NULL};
    if (EM_EXTRACT_BOOLEAN(func)) {
        EM_ASSERT_FUNCTION(func);
        callback = add_all_callback;
    }

    git_strarray pathspec;
    if (!egit_strarray_from_list(&pathspec, env, _pathspec))
        return esym_nil;

    int retval = git_index_add_all(index, &pathspec, options, callback, &payload);
    egit_strarray_dispose(&pathspec);

    EM_RETURN_NIL_IF_NLE();
    if (retval == GIT_EUSER)
        return esym_nil;
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(index_add_bypath, "INDEX PATH",
         "Add or update an entry in INDEX from a file on disk.\n"
         "Note: this does not obey ignore rules.");
emacs_value egit_index_add_bypath(emacs_env *env, emacs_value _index, emacs_value _path)
{
    EGIT_ASSERT_INDEX(_index);
    EM_ASSERT_STRING(_path);

    git_index *index = EGIT_EXTRACT(_index);
    char *path = EM_EXTRACT_STRING(_path);
    int retval = git_index_add_bypath(index, path);
    free(path);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(index_clear, "INDEX", "Clear the entries from an index.");
emacs_value egit_index_clear(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    int retval = git_index_clear(index);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(index_read, "INDEX &optional FORCE",
         "Update INDEX by reading from disk.\n"
         "If FORCE is non-nil, update even if no apparent changes are made.");
emacs_value egit_index_read(emacs_env *env, emacs_value _index, emacs_value force)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    int retval = git_index_read(index, EM_EXTRACT_BOOLEAN(force));
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(index_write, "INDEX", "Write the index to disk.");
emacs_value egit_index_write(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    int retval = git_index_write(index);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(index_write_tree, "INDEX &optional REPO",
         "Write the index to a tree and return the ID.\n"
         "If REPO is non-nil, write to that repository.");
emacs_value egit_index_write_tree(emacs_env *env, emacs_value _index, emacs_value _repo)
{
    EGIT_ASSERT_INDEX(_index);
    if (EM_EXTRACT_BOOLEAN(_repo))
        EGIT_ASSERT_REPOSITORY(_repo);

    git_index *index = EGIT_EXTRACT(_index);
    git_repository *repo = EGIT_EXTRACT_OR_NULL(_repo);

    git_oid oid;
    int retval = repo ? git_index_write_tree_to(&oid, index, repo)
                 : git_index_write_tree(&oid, index);
    EGIT_CHECK_ERROR(retval);
    const char *oid_s = git_oid_tostr_s(&oid);
    return EM_STRING(oid_s);
}
