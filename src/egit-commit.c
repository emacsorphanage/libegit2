#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-commit.h"


// =============================================================================
// Constructors

EGIT_DOC(commit_lookup, "REPO OID", "Look up a commit in REPO by OID.");
emacs_value egit_commit_lookup(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    git_commit *commit;
    int retval = git_commit_lookup(&commit, repo, &oid);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_COMMIT, commit);
}


// =============================================================================
// Getters

EGIT_DOC(commit_owner, "COMMIT", "Return the repository that COMMIT belongs to.");
emacs_value egit_commit_owner(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    git_repository *repo = git_commit_owner(commit);
    return egit_wrap_repository(env, repo);
}
