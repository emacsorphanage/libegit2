#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


EGIT_DOC(revparse, "REPO SPEC",
         "Return the object(s) referred to by spec.\n"
         "The return value is either a single object or a list (triplep FROM TO)\n"
         "where triplep indicates whether the .. or the ... operator was present.");
emacs_value egit_revparse(emacs_env *env, emacs_value _repo, emacs_value _spec)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_spec);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_revspec revspec;
    int retval;
    {
        char *spec = EM_EXTRACT_STRING(_spec);
        retval = git_revparse(&revspec, repo, spec);
        free(spec);
    }
    EGIT_CHECK_ERROR(retval);

    if (revspec.flags & GIT_REVPARSE_SINGLE)
        return egit_wrap(env, EGIT_OBJECT, revspec.from, EM_EXTRACT_USER_PTR(_repo));
    emacs_value ret;
    ret = em_cons(env, egit_wrap(env, EGIT_OBJECT, revspec.to, EM_EXTRACT_USER_PTR(_repo)), esym_nil);
    ret = em_cons(env, egit_wrap(env, EGIT_OBJECT, revspec.from, EM_EXTRACT_USER_PTR(_repo)), ret);
    ret = em_cons(env, revspec.flags & GIT_REVPARSE_MERGE_BASE ? esym_t : esym_nil, ret);
    return ret;
}

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
    emacs_value rref = esym_nil;
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
    if (retval == GIT_ENOTFOUND) {
      // We can only reach this if a giterr was not set by libgit.
      // This is a bug in libgit, bug report and fix are TBD.
      // Let's signal this error manually.
      emacs_value error = em_findenum_error(GITERR_REFERENCE);
      if (!EM_EXTRACT_BOOLEAN(error))
        error = esym_giterr;
      em_signal(env, error,
		"previously checked out branch or commit not found");
      return esym_nil;
    }

    return egit_wrap(env, EGIT_OBJECT, obj, EM_EXTRACT_USER_PTR(_repo));
}
