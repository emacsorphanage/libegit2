#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


EGIT_DOC(revparse_single, "REPO SPEC", "Return the object referred to by SPEC in REPO.");
emacs_value egit_revparse_single(emacs_env *env, emacs_value _repo, emacs_value _spec)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_spec);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_object *obj;
    int retval;
    {
        char *spec = EGIT_EXTRACT_STRING(_spec);
        retval = git_revparse_single(&obj, repo, spec);
        free(spec);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_OBJECT, obj);
}
