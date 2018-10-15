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
