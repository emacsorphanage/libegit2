#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-reflog.h"


// =============================================================================
// Constructors

EGIT_DOC(reflog_read, "REPO &optional REFNAME",
         "Open the reflog for REFNAME in REPO.\n"
         "If REFNAME is nil, open the reflog for HEAD.");
emacs_value egit_reflog_read(emacs_env *env, emacs_value _repo, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING_OR_NIL(_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *refname = EM_EXTRACT_STRING_OR_NULL(_refname);
    git_reflog *reflog;
    int retval = git_reflog_read(&reflog, repo, refname ? refname : "HEAD");
    free(refname);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFLOG, reflog, NULL);
}


// =============================================================================
// Getters

EGIT_DOC(reflog_entry_byindex, "REFLOG N", "Get the Nth entry in REFLOG.");
emacs_value egit_reflog_entry_byindex(emacs_env *env, emacs_value _reflog, emacs_value _index)
{
    EGIT_ASSERT_REFLOG(_reflog);
    EM_ASSERT_INTEGER(_index);
    git_reflog *reflog = EGIT_EXTRACT(_reflog);
    ptrdiff_t index = EM_EXTRACT_INTEGER(_index);
    const git_reflog_entry *entry = git_reflog_entry_byindex(reflog, index);

    if (!entry) {
        em_signal_args_out_of_range(env, index);
        return esym_nil;
    }

    return egit_wrap(env, EGIT_REFLOG_ENTRY, entry, EM_EXTRACT_USER_PTR(_reflog));
}

EGIT_DOC(reflog_entry_committer, "REFLOG-ENTRY", "Get the committer of REFLOG-ENTRY.");
emacs_value egit_reflog_entry_committer(emacs_env *env, emacs_value _entry)
{
    EGIT_ASSERT_REFLOG_ENTRY(_entry);
    const git_reflog_entry *entry = EGIT_EXTRACT(_entry);
    const git_signature *sig = git_reflog_entry_committer(entry);

    git_signature *new;
    int retval = git_signature_dup(&new, sig);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_SIGNATURE, new, NULL);
}

EGIT_DOC(reflog_entry_id, "REFLOG-ENTRY &optional NEW",
         "Get the ID of REFLOG-ENTRY.\n"
         "If NEW is non-nil, get the new side, otherwise the old side.");
emacs_value egit_reflog_entry_id(emacs_env *env, emacs_value _entry, emacs_value _new)
{
    EGIT_ASSERT_REFLOG_ENTRY(_entry);
    const git_reflog_entry *entry = EGIT_EXTRACT(_entry);

    const git_oid *oid;
    if (EM_EXTRACT_BOOLEAN(_new))
        oid = git_reflog_entry_id_new(entry);
    else
        oid = git_reflog_entry_id_old(entry);

    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(reflog_entry_message, "REFLOG-ENTRY", "Get the message of REFLOG-ENTRY.");
emacs_value egit_reflog_entry_message(emacs_env *env, emacs_value _entry)
{
    EGIT_ASSERT_REFLOG_ENTRY(_entry);
    const git_reflog_entry *entry = EGIT_EXTRACT(_entry);
    const char *msg = git_reflog_entry_message(entry);
    return EM_STRING(msg);
}

EGIT_DOC(reflog_entrycount, "REFLOG", "Get the number of entries in REFLOG");
emacs_value egit_reflog_entrycount(emacs_env *env, emacs_value _reflog)
{
    EGIT_ASSERT_REFLOG(_reflog);
    git_reflog *reflog = EGIT_EXTRACT(_reflog);
    return EM_INTEGER(git_reflog_entrycount(reflog));
}


// =============================================================================
// Operations

EGIT_DOC(reflog_append, "REFLOG ID COMMITTER &optional MESSAGE",
         "Add a new entry to REFLOG in memory.\n"
         "ID is the new object ID the reference points to, and COMMITTER\n"
         "is a signature object.");
emacs_value egit_reflog_append(
    emacs_env *env, emacs_value _reflog, emacs_value _id,
    emacs_value _committer, emacs_value _msg)
{
    EGIT_ASSERT_REFLOG(_reflog);
    EM_ASSERT_STRING(_id);
    EGIT_ASSERT_SIGNATURE(_committer);
    EM_ASSERT_STRING_OR_NIL(_msg);

    git_reflog *reflog = EGIT_EXTRACT(_reflog);
    git_signature *committer = EGIT_EXTRACT(_committer);
    char *msg = EM_EXTRACT_STRING_OR_NULL(_msg);

    git_oid id;
    EGIT_EXTRACT_OID(_id, id);

    int retval = git_reflog_append(reflog, &id, committer, msg);
    free(msg);

    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(reflog_delete, "REPO REFNAME", "Delete the reflog for REFNAME in REPO.");
emacs_value egit_reflog_delete(emacs_env *env, emacs_value _repo, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *refname = EM_EXTRACT_STRING(_refname);
    int retval = git_reflog_delete(repo, refname);
    free(refname);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(reflog_drop, "REFLOG N &optional REWRITE",
         "Delete the Nth entry from REFLOG.\n"
         "If REWRITE is non-nil, rewrite the history to ensure there's no gap,\n"
         "i.e. set the old ID of the next chronological entry to the new ID of\n"
         "the previous chronological entry.");
emacs_value egit_reflog_drop(emacs_env *env, emacs_value _reflog, emacs_value _index, emacs_value rewrite)
{
    EGIT_ASSERT_REFLOG(_reflog);
    EM_ASSERT_INTEGER(_index);
    git_reflog *reflog = EGIT_EXTRACT(_reflog);
    ptrdiff_t index = EM_EXTRACT_INTEGER(_index);

    int retval = git_reflog_drop(reflog, index, EM_EXTRACT_BOOLEAN(rewrite));
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(reflog_rename, "REPO OLD-REFNAME NEW-REFNAME",
         "Rename a reflog in REPO from OLD-REFNAME to NEW-REFNAME.");
emacs_value egit_reflog_rename(
    emacs_env *env, emacs_value _repo, emacs_value _old_refname, emacs_value _new_refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_old_refname);
    EM_ASSERT_STRING(_new_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *old_refname = EM_EXTRACT_STRING(_old_refname);
    char *new_refname = EM_EXTRACT_STRING(_new_refname);
    int retval = git_reflog_rename(repo, old_refname, new_refname);
    free(old_refname);
    free(new_refname);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(reflog_write, "REFLOG", "Write REFLOG back to disk.");
emacs_value egit_reflog_write(emacs_env *env, emacs_value _reflog)
{
    EGIT_ASSERT_REFLOG(_reflog);
    git_reflog *reflog = EGIT_EXTRACT(_reflog);
    int retval = git_reflog_write(reflog);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}
