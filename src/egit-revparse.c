#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


EGIT_DOC(revparse_ext, "REPO SPEC",
         "Find the object and reference (if applicable) referred to by SPEC in REPO.\n"
         "The return value is a cons cell where the car is an object and the cdr is\n"
         "a reference, or nil.");
emacs_value egit_revparse_ext(emacs_env *env, emacs_value _repo, emacs_value _spec)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_spec);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_object *obj;
    git_reference *ref;
    int retval;
    {
        char *spec = EM_EXTRACT_STRING(_spec);
        retval = git_revparse_ext(&obj, &ref, repo, spec);
        free(spec);
    }
    EGIT_CHECK_ERROR(retval);

    emacs_value robj = egit_wrap(env, EGIT_OBJECT, obj, EM_EXTRACT_USER_PTR(_repo));
    emacs_value rref = em_nil;
    if (ref)
        rref = egit_wrap(env, EGIT_REFERENCE, ref, EM_EXTRACT_USER_PTR(_repo));

    return em_cons(env, robj, rref);
}

EGIT_DOC(revparse_single, "REPO SPEC", "Return the object referred to by SPEC in REPO.");
emacs_value egit_revparse_single(emacs_env *env, emacs_value _repo, emacs_value _spec)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_spec);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_object *obj;
    int retval;
    {
        char *spec = EM_EXTRACT_STRING(_spec);
        retval = git_revparse_single(&obj, repo, spec);
        free(spec);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_OBJECT, obj, EM_EXTRACT_USER_PTR(_repo));
}
