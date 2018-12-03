#include <string.h>
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
    EM_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    git_commit *commit;
    int retval = git_commit_lookup(&commit, repo, &oid);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_COMMIT, commit, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(commit_lookup_prefix, "REPO OID", "Lookup a commit in REPO by shortened OID.");
emacs_value egit_commit_lookup_prefix(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    size_t len;
    EGIT_EXTRACT_OID_PREFIX(_oid, oid, len);

    git_commit *commit;
    int retval = git_commit_lookup_prefix(&commit, repo, &oid, len);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_COMMIT, commit, EM_EXTRACT_USER_PTR(_repo));
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

    return egit_wrap(env, EGIT_SIGNATURE, ret, NULL);
}

EGIT_DOC(commit_body, "COMMIT", "Get the message body of COMMIT (everything but the first paragraph).");
emacs_value egit_commit_body(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const char *body = git_commit_body(commit);
    return EM_STRING(body);
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

    return egit_wrap(env, EGIT_SIGNATURE, ret, NULL);
}

EGIT_DOC(commit_id, "COMMIT", "Return the ID of COMMIT.");
emacs_value egit_commit_id(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const git_oid *oid = git_commit_id(commit);
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(commit_message, "COMMIT", "Get the message of COMMIT.");
emacs_value egit_commit_message(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const char *message = git_commit_message(commit);
    return EM_STRING(message);
}

EGIT_DOC(commit_nth_gen_ancestor, "COMMIT N",
         "Return the Nth ancestor of COMMIT, following only first parents");
emacs_value egit_commit_nth_gen_ancestor(emacs_env *env, emacs_value _commit, emacs_value _n)
{
    EGIT_ASSERT_COMMIT(_commit);
    EM_ASSERT_INTEGER(_n);
    git_commit *commit = EGIT_EXTRACT(_commit);
    intmax_t n = EM_EXTRACT_INTEGER(_n);

    git_commit *ret;
    int retval = git_commit_nth_gen_ancestor(&ret, commit, n);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_COMMIT, ret, EGIT_EXTRACT_PARENT(_commit));
}

EGIT_DOC(commit_owner, "COMMIT", "Return the repository that COMMIT belongs to.");
emacs_value egit_commit_owner(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    egit_object *owner = EGIT_EXTRACT_PARENT(_commit);
    owner->refcount++;
    return EM_USER_PTR(owner, egit_finalize);
}

EGIT_DOC(commit_parent, "COMMIT &optional N", "Return the Nth parent of COMMIT.");
emacs_value egit_commit_parent(emacs_env *env, emacs_value _commit, emacs_value _n)
{
    EGIT_ASSERT_COMMIT(_commit);
    EM_ASSERT_INTEGER_OR_NIL(_n);
    git_commit *commit = EGIT_EXTRACT(_commit);
    intmax_t n = EM_EXTRACT_INTEGER_OR_DEFAULT(_n, 0);

    git_commit *ret;
    int retval = git_commit_parent(&ret, commit, n);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_COMMIT, ret, EGIT_EXTRACT_PARENT(_commit));
}

EGIT_DOC(commit_parent_id, "COMMIT &optional N", "Return the ID of the Nth parent of COMMIT.");
emacs_value egit_commit_parent_id(emacs_env *env, emacs_value _commit, emacs_value _n)
{
    EGIT_ASSERT_COMMIT(_commit);
    EM_ASSERT_INTEGER_OR_NIL(_n);
    git_commit *commit = EGIT_EXTRACT(_commit);
    intmax_t n = EM_EXTRACT_INTEGER_OR_DEFAULT(_n, 0);

    const git_oid *oid = git_commit_parent_id(commit, n);
    if (!oid) {
        em_signal_args_out_of_range(env, n);
        return esym_nil;
    }

    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(commit_parentcount, "COMMIT", "Return the number of parents of COMMIT.");
emacs_value egit_commit_parentcount(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    unsigned int count = git_commit_parentcount(commit);
    return EM_INTEGER(count);
}

EGIT_DOC(commit_summary, "COMMIT", "Get the summary of COMMIT (the first paragraph of the message).");
emacs_value egit_commit_summary(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const char *summary = git_commit_summary(commit);
    return EM_STRING(summary);
}

EGIT_DOC(commit_time, "COMMIT", "Get the time COMMIT was authored.");
emacs_value egit_commit_time(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    return em_decode_time(env, git_commit_time(commit), git_commit_time_offset(commit) * 60);
}

EGIT_DOC(commit_tree, "COMMIT", "Get the tree associated with COMMIT.");
emacs_value egit_commit_tree(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    git_tree *tree;
    int retval = git_commit_tree(&tree, commit);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_TREE, tree, EGIT_EXTRACT_PARENT(_commit));
}

EGIT_DOC(commit_tree_id, "COMMIT", "Get the ID of the tree associated with COMMIT.");
emacs_value egit_commit_tree_id(emacs_env *env, emacs_value _commit)
{
    EGIT_ASSERT_COMMIT(_commit);
    git_commit *commit = EGIT_EXTRACT(_commit);
    const git_oid *oid = git_commit_tree_id(commit);
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}


// =============================================================================
// Operations

EGIT_DOC(commit_create, "REPO REFNAME AUTHOR COMMITTER MESSAGE TREE &optional PARENTS",
         "Create a new committ in REPO and return its ID.\n"
         "REFNAME, if non-nil, must be the name of a reference that will\n"
         "be updated to point to the new commit. It will be created if it\n"
         "does not exist. Use \"HEAD\" to update the HEAD of the current branch.\n"
         "AUTHOR and COMMITTER must be signature objects. MESSAGE is the commit\n"
         "message. MESSAGE will not be cleaned up, see `libgit-message-prettify'.\n"
         "TREE is the root tree object associated with the new commit, and PARENTS\n"
         "is a list of parent commits.\n\n"
         "TREE and all PARENTS must be owned by REPO. If REFNAME exists, the first\n"
         "parent must be the tip of that branch.");
emacs_value egit_commit_create(
    emacs_env *env, emacs_value _repo, emacs_value _refname, emacs_value _author,
    emacs_value _committer, emacs_value _msg, emacs_value _tree, emacs_value _parents)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING_OR_NIL(_refname);
    EGIT_ASSERT_SIGNATURE(_author);
    EGIT_ASSERT_SIGNATURE(_committer);
    EM_ASSERT_STRING(_msg);
    EGIT_ASSERT_TREE(_tree);

    ptrdiff_t i = 0, nparents = egit_assert_list(env, EGIT_COMMIT, esym_libgit_commit_p, _parents);
    if (nparents < 0)
        return esym_nil;
    const git_commit *parents[nparents];
    {
        EM_DOLIST(p, _parents, get_parents);
        parents[i++] = EGIT_EXTRACT(p);
        EM_DOLIST_END(get_parents);
    }

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *refname = EM_EXTRACT_STRING_OR_NULL(_refname);
    git_signature *author = EGIT_EXTRACT(_author);
    git_signature *committer = EGIT_EXTRACT(_committer);
    char *msg = EM_EXTRACT_STRING(_msg);
    git_tree *tree = EGIT_EXTRACT(_tree);

    git_oid oid;
    int retval = git_commit_create(&oid, repo, refname, author, committer, NULL, msg, tree, nparents, parents);
    free(refname);
    free(msg);
    EGIT_CHECK_ERROR(retval);

    const char *oid_s = git_oid_tostr_s(&oid);
    return EM_STRING(oid_s);
}
