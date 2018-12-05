#include <string.h>
#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"

EGIT_DOC(annotated_commit_from_ref, "REPO REF",
         "Return an annotated commit for the given REF.");
emacs_value egit_annotated_commit_from_ref(emacs_env *env,
                                           emacs_value _repo,
                                           emacs_value _ref)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_REFERENCE(_ref);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_reference *ref = EGIT_EXTRACT(_ref);

    git_annotated_commit *ann_commit = NULL;
    int retval = git_annotated_commit_from_ref(&ann_commit, repo, ref);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_ANNOTATED_COMMIT, ann_commit, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(annotated_commit_from_fetchhead, "REPO BRANCH-NAME REMOTE-URL ID",
         "Return an annotated commit for the given fetch head data.");
emacs_value egit_annotated_commit_from_fetchhead(emacs_env *env,
                                                 emacs_value _repo,
                                                 emacs_value _branch_name,
                                                 emacs_value _remote_url,
                                                 emacs_value _id)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_branch_name);
    EM_ASSERT_STRING(_remote_url);
    EM_ASSERT_STRING(_id);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char* branch_name = EM_EXTRACT_STRING(_branch_name);
    char* remote_url  = EM_EXTRACT_STRING(_remote_url);
    git_oid id;
    EGIT_EXTRACT_OID(_id, id);

    git_annotated_commit *ann = NULL;
    int retval = git_annotated_commit_from_fetchhead(&ann, repo, branch_name,
                                                     remote_url, &id);
    free(branch_name);
    free(remote_url);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_ANNOTATED_COMMIT, ann, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(annotated_commit_from_revspec, "REPO REVSPEC",
         "Return an annotated commit for the given revision string REVSPEC.");
emacs_value egit_annotated_commit_from_revspec(emacs_env *env,
                                               emacs_value _repo,
                                               emacs_value _spec)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_spec);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_annotated_commit *ann = NULL;
    int retval;
    {
        char *spec = EM_EXTRACT_STRING(_spec);
        retval = git_annotated_commit_from_revspec(&ann, repo, spec);
        free(spec);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_ANNOTATED_COMMIT, ann, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(annotated_commit_lookup, "REPO ID",
         "Return an annotated commit for the given commit ID.");
emacs_value egit_annotated_commit_lookup(emacs_env *env,
                                         emacs_value _repo,
                                         emacs_value _id)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_id);

    git_annotated_commit *ann = NULL;
    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    EGIT_EXTRACT_OID(_id, oid);

    int retval = git_annotated_commit_lookup(&ann, repo, &oid);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_ANNOTATED_COMMIT, ann, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(annotated_commit_id, "ANNOTATED-COMMIT",
         "Return a commit id for the given ANNOTATED-COMMIT.");
emacs_value egit_annotated_commit_id(emacs_env *env,
                                     emacs_value _ann)
{
    EGIT_ASSERT_ANNOTATED_COMMIT(_ann);

    git_annotated_commit *ann = EGIT_EXTRACT(_ann);
    const git_oid *oid = git_annotated_commit_id(ann);

    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}
