#include <stdio.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-blob.h"


// =============================================================================
// Constructors

EGIT_DOC(blob_lookup, "REPO OID", "Look up a blob in REPO by OID.");
emacs_value egit_blob_lookup(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    git_blob *blob;
    int retval = git_blob_lookup(&blob, repo, &oid);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_BLOB, blob, NULL);
}

EGIT_DOC(blob_lookup_prefix, "REPO OID", "Lookup a blob in REPO by shortened OID.");
emacs_value egit_blob_lookup_prefix(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    size_t len;
    EGIT_EXTRACT_OID_PREFIX(_oid, oid, len);

    git_blob *blob;
    int retval = git_blob_lookup_prefix(&blob, repo, &oid, len);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_BLOB, blob, NULL);
}


// =============================================================================
// Getters

EGIT_DOC(blob_id, "BLOB", "Return the ID of BLOB.");
emacs_value egit_blob_id(emacs_env *env, emacs_value _blob)
{
    EGIT_ASSERT_BLOB(_blob);
    git_blob *blob = EGIT_EXTRACT(_blob);
    const git_oid *oid = git_blob_id(blob);
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}
