#include "interface.h"
#include "egit-branch.h"


EGIT_DOC(branch_create, "REPO NAME COMMITISH FORCE",
         "Create a new branch in REPO named NAME at COMMITISH and return the reference to it.\n\n"
         "If FORCE is non-nil, force creation of the branch.");
emacs_value egit_branch_create(emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _commitish, emacs_value _force)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_commitish);
    EM_ASSERT_STRING(_name);

    git_repository *repo = EGIT_EXTRACT(_repo);
    
    git_reference *target_ref;
    int retval;
    {
        char *commitish = EM_EXTRACT_STRING(_commitish);
        retval = git_reference_dwim(&target_ref, repo, commitish);
        free(commitish);
    }
    EGIT_CHECK_ERROR(retval);
    
    const git_oid *oid = git_reference_target(target_ref);

    // TODO: Deal with this more robustly
    if (!oid) {
        em_signal_giterr(env, 1, "Reference is not direct");
        git_reference_free(target_ref);
        return em_nil;
    }

    git_commit *commit;
    retval = git_commit_lookup(&commit, repo, oid);
    git_reference_free(target_ref);
    EGIT_CHECK_ERROR(retval);

    git_reference *ref;
    {
        char *name = EM_EXTRACT_STRING(_name);
        bool force = EM_EXTRACT_BOOLEAN(_force);
        retval = git_branch_create(&ref, repo, name, commit, force);
        free(name);
    }
    git_commit_free(commit);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref, NULL);
}

EGIT_DOC(branch_create_from_annotated, "REPO NAME COMMITISH FORCE",
         "Create a new branch in REPO named NAME at annotated commit COMMITISH and return the reference to it.\n\n"
         "If FORCE is non-nil, force creation of the branch.");
emacs_value egit_branch_create_from_annotated(
    emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _commitish, emacs_value _force)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_commitish);
    EM_ASSERT_STRING(_name);

    git_repository *repo = EGIT_EXTRACT(_repo);
    
    git_reference *target_ref;
    int retval;
    {
        char *commitish = EM_EXTRACT_STRING(_commitish);
        retval = git_reference_dwim(&target_ref, repo, commitish);
        free(commitish);
    }
    EGIT_CHECK_ERROR(retval);
    
    const git_oid *oid = git_reference_target(target_ref);

    // TODO: Deal with this more robustly
    if (!oid) {
        em_signal_giterr(env, 1, "Reference is not direct");
        git_reference_free(target_ref);
        return em_nil;
    }

    git_annotated_commit *commit;
    retval = git_annotated_commit_lookup(&commit, repo, oid);
    git_reference_free(target_ref);
    EGIT_CHECK_ERROR(retval);

    git_reference *ref;
    {
        char *name = EM_EXTRACT_STRING(_name);
        bool force = EM_EXTRACT_BOOLEAN(_force);
        retval = git_branch_create_from_annotated(&ref, repo, name, commit, force);
        free(name);
    }
    git_annotated_commit_free(commit);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref, NULL);
}

EGIT_DOC(branch_lookup, "REPO NAME &optional REMOTE",
         "Lookup branch named NAME in REPO.\n\nIf REMOTE is non-nil, look for a remote branch.");
emacs_value egit_branch_lookup(emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _remote)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);

    git_repository *repo = EGIT_EXTRACT(_repo);

    git_reference *target_ref;
    int retval;
    {
        char *branch = EM_EXTRACT_STRING(_name);
        int remote = EM_EXTRACT_BOOLEAN(_remote);
        retval = git_branch_lookup(&target_ref, repo, branch, remote ? GIT_BRANCH_REMOTE : GIT_BRANCH_LOCAL);
        free(branch);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, target_ref, NULL);
}

EGIT_DOC(branch_delete, "REF", "Delete branch at REF.");
emacs_value egit_branch_delete(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);

    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_branch_delete(ref);
    EGIT_CHECK_ERROR(retval);

    return em_nil;
}

EGIT_DOC(branch_checked_out_p, "REF", "Check if branch at REF is checked out.");
emacs_value egit_branch_checked_out_p(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);

    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_branch_is_checked_out(ref);
    EGIT_CHECK_ERROR(retval);

    return retval ? em_t : em_nil;
}

EGIT_DOC(branch_head_p, "REF", "Check if branch at REF is HEAD.");
emacs_value egit_branch_head_p(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);

    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_branch_is_head(ref);
    EGIT_CHECK_ERROR(retval);

    return retval ? em_t : em_nil;
}

EGIT_DOC(branch_name, "REF", "Return the name of the branch at REF.");
emacs_value egit_branch_name(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);

    const git_reference *ref = EGIT_EXTRACT(_ref);
    const char *name;

    const int retval = git_branch_name(&name, ref);
    EGIT_CHECK_ERROR(retval);

    return env->make_string(env, name, strlen(name));
}

EGIT_DOC(branch_remote_name, "REPO BRANCH",
         "Return the name of the remote of a remote-tracking BRANCH.\n");
emacs_value egit_branch_remote_name(emacs_env *env, emacs_value _repo, emacs_value _branch)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_branch);

    git_repository *const repo = EGIT_EXTRACT(_repo);
    char *const branch = EM_EXTRACT_STRING(_branch);
    git_buf remote_name = {NULL, 0, 0};

    const int retval = git_branch_remote_name(&remote_name, repo, branch);

    free(branch);
    EGIT_CHECK_ERROR(retval);

    EGIT_RET_BUF_AS_STRING(remote_name);
}

EGIT_DOC(branch_upstream_name, "REPO REFNAME",
         "Return the name of the reference of a remote-tracking branch.\n"
         "REFNAME is a local branch reference in REPO.");
emacs_value egit_branch_upstream_name(emacs_env *env, emacs_value _repo, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_refname);

    git_repository *const repo = EGIT_EXTRACT(_repo);
    char *const refname = EM_EXTRACT_STRING(_refname);
    git_buf upstream_name = {NULL, 0, 0};

    const int retval = git_branch_upstream_name(&upstream_name, repo, refname);

    free(refname);
    EGIT_CHECK_ERROR(retval);

    EGIT_RET_BUF_AS_STRING(upstream_name);
}
