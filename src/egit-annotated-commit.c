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

    return egit_wrap(env, EGIT_ANNOTATED_COMMIT, ann_commit, NULL);
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

    return egit_wrap(env, EGIT_ANNOTATED_COMMIT, ann, NULL);
}
