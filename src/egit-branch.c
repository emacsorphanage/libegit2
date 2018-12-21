#include <string.h>

#include "interface.h"
#include "egit-branch.h"


EGIT_DOC(branch_create, "REPO NAME COMMIT &optional FORCE",
         "Create a new branch in REPO named NAME at COMMIT and return the reference to it.\n\n"
         "If FORCE is non-nil, force creation of the branch.");
emacs_value egit_branch_create(
    emacs_env *env, emacs_value _repo, emacs_value _name,
    emacs_value _commit, emacs_value _force)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_COMMIT(_commit);
    EM_ASSERT_STRING(_name);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_commit *commit = EGIT_EXTRACT(_commit);
    char *name = EM_EXTRACT_STRING(_name);
    bool force = EM_EXTRACT_BOOLEAN(_force);

    git_reference *ref;
    int retval = git_branch_create(&ref, repo, name, commit, force);
    free(name);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ref, EM_EXTRACT_USER_PTR(_repo));
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
        em_signal(env, esym_giterr, "Reference is not direct");
        git_reference_free(target_ref);
        return esym_nil;
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

    return egit_wrap(env, EGIT_REFERENCE, ref, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(branch_foreach, "REPO TYPE FUNC",
         "Run FUNC for each branch in REPO.\n"
         "FUNC is called with one argument, which is a reference object.\n"
         "TYPE may be either `local', `remote' or `all'.");
emacs_value egit_branch_foreach(emacs_env *env, emacs_value _repo, emacs_value _type, emacs_value func)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_FUNCTION(func);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_branch_t type;
    if (!em_findsym_branch(&type, env, _type, true))
        return esym_nil;

    git_branch_iterator *iter;
    int retval = git_branch_iterator_new(&iter, repo, type);
    EGIT_CHECK_ERROR(retval);

    git_reference *out;
    git_branch_t out_type;
    while (true) {
        int retval = git_branch_next(&out, &out_type, iter);
        if (retval != 0) {
            git_branch_iterator_free(iter);
            if (retval == GIT_ITEROVER)
                return esym_nil;
            EGIT_CHECK_ERROR(retval);
            return esym_nil;  // Should be unreachable
        }

        emacs_value wrap = egit_wrap(env, EGIT_REFERENCE, out, EM_EXTRACT_USER_PTR(_repo));
        env->funcall(env, func, 1, &wrap);

        if (env->non_local_exit_check(env)) {
            git_branch_iterator_free(iter);
            return esym_nil;
        }
    }
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

    return egit_wrap(env, EGIT_REFERENCE, target_ref, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(branch_delete, "REF", "Delete branch at REF.");
emacs_value egit_branch_delete(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);

    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_branch_delete(ref);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(branch_checked_out_p, "REF", "Check if branch at REF is checked out.");
emacs_value egit_branch_checked_out_p(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);

    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_branch_is_checked_out(ref);
    EGIT_CHECK_ERROR(retval);

    return retval ? esym_t : esym_nil;
}

EGIT_DOC(branch_head_p, "REF", "Check if branch at REF is HEAD.");
emacs_value egit_branch_head_p(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);

    git_reference *ref = EGIT_EXTRACT(_ref);
    int retval = git_branch_is_head(ref);
    EGIT_CHECK_ERROR(retval);

    return retval ? esym_t : esym_nil;
}

EGIT_DOC(branch_move, "REF NEWNAME &optional FORCE",
         "Move REF to NEWNAME.\n"
         "If FORCE is non-nil, overwrite existing branches.\n"
         "Return the renamed reference.");
emacs_value egit_branch_move(emacs_env *env, emacs_value _ref,
                             emacs_value _newname, emacs_value force)
{
    EGIT_ASSERT_REFERENCE(_ref);
    EM_ASSERT_STRING(_newname);

    git_reference *ref = EGIT_EXTRACT(_ref);
    char *newname = EM_EXTRACT_STRING(_newname);;

    git_reference *out;
    int retval = git_branch_move(&out, ref, newname, EM_EXTRACT_BOOLEAN(force));
    free(newname);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, out, EGIT_EXTRACT_PARENT(_ref));
}

EGIT_DOC(branch_name, "REF", "Return the name of the branch at REF.");
emacs_value egit_branch_name(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);

    const git_reference *ref = EGIT_EXTRACT(_ref);
    const char *name;

    const int retval = git_branch_name(&name, ref);
    EGIT_CHECK_ERROR(retval);

    return EM_STRING(name);
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

EGIT_DOC(branch_set_upstream, "REF REFNAME",
         "Set REFNAME as the upstream of the branch REF.");
emacs_value egit_branch_set_upstream(emacs_env *env, emacs_value _ref, emacs_value _refname)
{
    EGIT_ASSERT_REFERENCE(_ref);
    EM_ASSERT_STRING_OR_NIL(_refname);

    git_reference *ref = EGIT_EXTRACT(_ref);
    char *refname = EM_EXTRACT_STRING_OR_NULL(_refname);

    int retval = git_branch_set_upstream(ref, refname);
    free(refname);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(branch_upstream, "REF",
         "Return the reference of a remote-tracking branch.\n"
         "REF is a local branch reference in REPO.");
emacs_value egit_branch_upstream(emacs_env *env, emacs_value _ref)
{
    EGIT_ASSERT_REFERENCE(_ref);
    git_reference *ref = EGIT_EXTRACT(_ref);

    git_reference *ret;
    int retval = git_branch_upstream(&ret, ref);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFERENCE, ret, EGIT_EXTRACT_PARENT(_ref));
}

EGIT_DOC(branch_upstream_name, "REPO REFNAME",
         "Return the name of the reference of a remote-tracking branch.\n"
         "REFNAME is a local branch reference name in REPO.");
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

EGIT_DOC(branch_upstream_remote, "REPO REFNAME",
         "Return the name of the upstream remote of local branch REFNAME in REPO.");
emacs_value egit_branch_upstream_remote(emacs_env *env, emacs_value _repo, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *refname = EM_EXTRACT_STRING(_refname);

    git_buf buf = {0};
    int retval = git_branch_upstream_remote(&buf, repo, refname);
    free(refname);
    EGIT_CHECK_ERROR(retval);

    EGIT_RET_BUF_AS_STRING(buf);
}
