#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-index.h"


// =============================================================================
// Helpers

static git_index_entry *egit_index_entry_dup(const git_index_entry *index)
{
    git_index_entry *new = (git_index_entry*) malloc(sizeof(git_index_entry));
    memcpy(new, index, sizeof(git_index_entry));
    new->path = strdup(index->path);
    return new;
}


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

    emacs_value ret = em_nil;
    if (caps & GIT_INDEXCAP_FROM_OWNER)
        ret = em_cons(env, em_from_owner, ret);
    if (caps & GIT_INDEXCAP_NO_SYMLINKS)
        ret = em_cons(env, em_no_symlinks, ret);
    if (caps & GIT_INDEXCAP_NO_FILEMODE)
        ret = em_cons(env, em_no_filemode, ret);
    if (caps & GIT_INDEXCAP_IGNORE_CASE)
        ret = em_cons(env, em_ignore_case, ret);

    return ret;
}

EGIT_DOC(index_checksum, "INDEX", "Get the checksum of INDEX.");
emacs_value egit_index_checksum(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    const git_oid *oid = git_index_checksum(index);
    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
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
    EGIT_ASSERT_FUNCTION(function);
    git_index *index = EGIT_EXTRACT(_index);
    git_index_conflict_iterator *iter;
    int retval = git_index_conflict_iterator_new(&iter, index);
    EGIT_CHECK_ERROR(retval);

    const git_index_entry *base, *ours, *theirs;
    while (true) {
        int retval = git_index_conflict_next(&base, &ours, &theirs, iter);
        if (retval != 0) {
            git_index_conflict_iterator_free(iter);
            if (retval == GIT_ITEROVER)
                return em_nil;
            EGIT_CHECK_ERROR(retval);
            return em_nil;  // Should be unreachable
        }

        emacs_value args[4];
        args[0] = env->make_string(env, base->path, strlen(base->path));
        args[1] = egit_wrap(env, EGIT_INDEX_ENTRY, egit_index_entry_dup(base));
        args[2] = egit_wrap(env, EGIT_INDEX_ENTRY, egit_index_entry_dup(ours));
        args[3] = egit_wrap(env, EGIT_INDEX_ENTRY, egit_index_entry_dup(theirs));
        env->funcall(env, function, 4, args);

        if (env->non_local_exit_check(env))
            return em_nil;
    }
}

EGIT_DOC(index_conflict_get, "INDEX PATH",
         "Get the INDEX entries associated with a conflict at PATH.\n"
         "Returns a list with three elements: the base entry, our and their side.");
emacs_value egit_index_conflict_get(emacs_env *env, emacs_value _index, emacs_value _path)
{
    EGIT_ASSERT_INDEX(_index);
    EGIT_ASSERT_STRING(_path);
    git_index *index = EGIT_EXTRACT(_index);
    char *path = EGIT_EXTRACT_STRING(_path);
    const git_index_entry *base, *ours, *theirs;
    int retval = git_index_conflict_get(&base, &ours, &theirs, index, path);
    free(path);
    EGIT_CHECK_ERROR(retval);

    emacs_value ret[3];
    ret[0] = egit_wrap(env, EGIT_INDEX_ENTRY, egit_index_entry_dup(base));
    ret[1] = egit_wrap(env, EGIT_INDEX_ENTRY, egit_index_entry_dup(ours));
    ret[2] = egit_wrap(env, EGIT_INDEX_ENTRY, egit_index_entry_dup(theirs));
    return em_list(env, ret, 3);
}

EGIT_DOC(index_entry_id, "ENTRY", "Get the path of the given index ENTRY.");
emacs_value egit_index_entry_id(emacs_env *env, emacs_value _entry)
{
    EGIT_ASSERT_INDEX_ENTRY(_entry);
    git_index_entry *entry = EGIT_EXTRACT(_entry);
    const char *oid_s = git_oid_tostr_s(&entry->id);
    return env->make_string(env, oid_s, strlen(oid_s));
}

EGIT_DOC(index_entry_path, "ENTRY", "Get the path of the given index ENTRY.");
emacs_value egit_index_entry_path(emacs_env *env, emacs_value _entry)
{
    EGIT_ASSERT_INDEX_ENTRY(_entry);
    git_index_entry *entry = EGIT_EXTRACT(_entry);
    return env->make_string(env, entry->path, strlen(entry->path));
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
    switch (stage) {
    case 0: return em_nil;
    case 1: return em_base;
    case 2: return em_ours;
    case 3: return em_theirs;
    }

    // Should be unreachable
    return em_nil;
}

EGIT_DOC(index_entrycount, "INDEX", "Get the number of entries in INDEX.");
emacs_value egit_index_entrycount(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    size_t count = git_index_entrycount(index);
    return env->make_integer(env, count);
}

EGIT_DOC(index_get_byindex, "INDEX N", "Get the Nth entry in INDEX.");
emacs_value egit_index_get_byindex(emacs_env *env, emacs_value _index, emacs_value _n)
{
    EGIT_ASSERT_INDEX(_index);
    EGIT_ASSERT_INTEGER(_n);
    git_index *index = EGIT_EXTRACT(_index);
    intmax_t n = EGIT_EXTRACT_INTEGER(_n);
    const git_index_entry *entry = git_index_get_byindex(index, n);
    if (!entry) {
        em_signal_args_out_of_range(env, n);
        return em_nil;
    }
    return egit_wrap(env, EGIT_INDEX_ENTRY, egit_index_entry_dup(entry));
}

EGIT_DOC(index_get_bypath, "INDEX PATH &optional STAGE",
         "Get an entry from INDEX by PATH.\n"
         "STAGE may be one of the symbols `base', `ours', `theirs',\n"
         "in which case look for an entry at the given stage.\n"
         "If STAGE is nil, look for a conflict-less entry.");
emacs_value egit_index_get_bypath(emacs_env *env, emacs_value _index, emacs_value _path, emacs_value _stage)
{
    EGIT_ASSERT_INDEX(_index);
    EGIT_ASSERT_STRING(_path);

    int stage;
    if (!EGIT_EXTRACT_BOOLEAN(_stage))
        stage = 0;
    else if (env->eq(env, _stage, em_base))
        stage = 1;
    else if (env->eq(env, _stage, em_ours))
        stage = 2;
    else if (env->eq(env, _stage, em_theirs))
        stage = 3;
    else {
        em_signal_wrong_value(env, _stage);
        return em_nil;
    }

    git_index *index = EGIT_EXTRACT(_index);
    char *path = EGIT_EXTRACT_STRING(_path);
    const git_index_entry *entry = git_index_get_bypath(index, path, stage);
    free(path);

    if (!entry)
        return em_nil; // TODO: Better to signal an error?
    return egit_wrap(env, EGIT_INDEX_ENTRY, egit_index_entry_dup(entry));
}

EGIT_DOC(index_owner, "INDEX", "Return the repository associated with INDEX.");
emacs_value egit_index_owner(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    git_repository *repo = git_index_owner(index);
    return egit_wrap(env, EGIT_REPOSITORY, repo);
}

EGIT_DOC(index_path, "INDEX", "Get the path to the index file on disk.");
emacs_value egit_index_path(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    const char *path = git_index_path(index);
    if (!path)
        return em_nil;
    return env->make_string(env, path, strlen(path));
}

EGIT_DOC(index_version, "INDEX", "Return the version of INDEX.");
emacs_value egit_index_version(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    unsigned int version = git_index_version(index);
    return env->make_integer(env, version);
}


// =============================================================================
// Predicates

EGIT_DOC(index_conflicts_p, "INDEX", "Return non-nil if INDEX has conflicts.");
emacs_value egit_index_conflicts_p(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    return git_index_has_conflicts(index) ? em_t : em_nil;
}
