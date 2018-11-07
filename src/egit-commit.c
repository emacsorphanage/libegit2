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

EGIT_DOC(commit_lookup_prefix, "REPO OID", "Lookup a commit in REPO by shortened OID.");
emacs_value egit_commit_lookup_prefix(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    size_t len;
    EGIT_EXTRACT_OID_PREFIX(_oid, oid, len);

    git_commit *commit;
    int retval = git_commit_lookup_prefix(&commit, repo, &oid, len);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_COMMIT, commit);
}


// =============================================================================
// Getters

EGIT_DOC(commit_author, "COMMIT", "Return the author of COMMIT as a signature object.");
emacs_value egit_commit_author(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const git_signature *sig = git_commit_author(commit);

    // Copy the signature so it can live independently from the commit
    git_signature *ret;
    int retval = git_signature_dup(&ret, sig);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_SIGNATURE, ret);
}

EGIT_DOC(commit_committer, "COMMIT", "Return the committer of COMMIT as a signature object.");
emacs_value egit_commit_committer(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const git_signature *sig = git_commit_committer(commit);

    // Copy the signature so it can live independently from the commit
    git_signature *ret;
    int retval = git_signature_dup(&ret, sig);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_SIGNATURE, ret);
}

EGIT_DOC(commit_id, "COMMIT", "Return the ID of COMMIT.");
emacs_value egit_commit_id(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const git_oid *oid = git_commit_id(commit);
    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}

EGIT_DOC(commit_message, "COMMIT", "Get the message of COMMIT.");
emacs_value egit_commit_message(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const char *message = git_commit_message(commit);
    return env->make_string(env, message, strlen(message));
}

EGIT_DOC(commit_nth_gen_ancestor, "COMMIT N",
         "Return the Nth ancestor of COMMIT, following only first parents");
emacs_value egit_commit_nth_gen_ancestor(emacs_env *env, emacs_value _commit, emacs_value _n)
{
    EGIT_ASSERT_COMMIT(_commit);
    EGIT_ASSERT_INTEGER(_n);
    git_commit *commit = EGIT_EXTRACT(_commit);
    intmax_t n = EGIT_EXTRACT_INTEGER(_n);

    git_commit *ret;
    int retval = git_commit_nth_gen_ancestor(&ret, commit, n);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_COMMIT, ret);
}

EGIT_DOC(commit_owner, "COMMIT", "Return the repository that COMMIT belongs to.");
emacs_value egit_commit_owner(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    git_repository *repo = git_commit_owner(commit);
    return egit_wrap_repository(env, repo);
}

EGIT_DOC(commit_parent, "COMMIT &optional N", "Return the Nth parent of COMMIT.");
emacs_value egit_commit_parent(emacs_env *env, emacs_value _commit, emacs_value _n)
{
    EGIT_ASSERT_COMMIT(_commit);
    EGIT_ASSERT_INTEGER_OR_NIL(_n);
    git_commit *commit = EGIT_EXTRACT(_commit);
    intmax_t n = EGIT_EXTRACT_INTEGER_OR_DEFAULT(_n, 0);

    git_commit *ret;
    int retval = git_commit_parent(&ret, commit, n);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_COMMIT, ret);
}

EGIT_DOC(commit_parent_id, "COMMIT &optional N", "Return the ID of the Nth parent of COMMIT.");
emacs_value egit_commit_parent_id(emacs_env *env, emacs_value _commit, emacs_value _n)
{
    EGIT_ASSERT_COMMIT(_commit);
    EGIT_ASSERT_INTEGER_OR_NIL(_n);
    git_commit *commit = EGIT_EXTRACT(_commit);
    intmax_t n = EGIT_EXTRACT_INTEGER_OR_DEFAULT(_n, 0);

    const git_oid *oid = git_commit_parent_id(commit, n);
    if (!oid) {
        em_signal_args_out_of_range(env, n);
        return em_nil;
    }

    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}

EGIT_DOC(commit_parentcount, "COMMIT", "Return the number of parents of COMMIT.");
emacs_value egit_commit_parentcount(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    unsigned int count = git_commit_parentcount(commit);
    return env->make_integer(env, count);
}

EGIT_DOC(commit_summary, "COMMIT", "Get the summary of COMMIT (the first paragraph of the message).");
emacs_value egit_commit_summary(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const char *summary = git_commit_summary(commit);
    return env->make_string(env, summary, strlen(summary));
}

EGIT_DOC(commit_tree, "COMMIT", "Get the tree associated with COMMIT.");
emacs_value egit_commit_tree(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    git_tree *tree;
    int retval = git_commit_tree(&tree, commit);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_TREE, tree);
}

EGIT_DOC(commit_tree_id, "COMMIT", "Get the ID of the tree associated with COMMIT.");
emacs_value egit_commit_tree_id(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const git_oid *oid = git_commit_tree_id(commit);
    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}
