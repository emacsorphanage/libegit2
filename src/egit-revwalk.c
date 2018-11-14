#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-revwalk.h"


// =============================================================================
// Constructors

EGIT_DOC(revwalk_new, "REPO", "Create a new revision walker for REPO.");
emacs_value egit_revwalk_new(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);
    git_revwalk *revwalk;
    int retval = git_revwalk_new(&revwalk, repo);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_REVWALK, revwalk, EM_EXTRACT_USER_PTR(_repo));
}


// =============================================================================
// Getters

EGIT_DOC(revwalk_repository, "REVWALK", "Return the repository associated with REVWALK");
emacs_value egit_revwalk_repository(emacs_env *env, emacs_value _revwalk)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    egit_object *owner = EGIT_EXTRACT_PARENT(_revwalk);
    owner->refcount++;
    return EM_USER_PTR(owner, egit_finalize);
}


// =============================================================================
// Hide and push

EGIT_DOC(revwalk_hide, "REVWALK OID",
         "Mark commit OID and its ancestors as uninteresting for REVWALK.");
emacs_value egit_revwalk_hide(emacs_env *env, emacs_value _revwalk, emacs_value _oid)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    EM_ASSERT_STRING(_oid);

    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    int retval = git_revwalk_hide(revwalk, &oid);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}

EGIT_DOC(revwalk_hide_glob, "REVWALK GLOB", "Hide references matched by GLOB in REVWALK.");
emacs_value egit_revwalk_hide_glob(emacs_env *env, emacs_value _revwalk, emacs_value _glob)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    EM_ASSERT_STRING(_glob);

    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    char *glob = EM_EXTRACT_STRING(_glob);

    int retval = git_revwalk_hide_glob(revwalk, glob);
    free(glob);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}

EGIT_DOC(revwalk_hide_head, "REVWALK", "Hide the repository HEAD in REVWALK.");
emacs_value egit_revwalk_hide_head(emacs_env *env, emacs_value _revwalk)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    int retval = git_revwalk_hide_head(revwalk);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}

EGIT_DOC(revwalk_hide_ref, "REVWALK REFNAME", "Hide the OID pointed to by REFNAME in REVWALK.");
emacs_value egit_revwalk_hide_ref(emacs_env *env, emacs_value _revwalk, emacs_value _refname)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    EM_ASSERT_STRING(_refname);

    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    char *refname = EM_EXTRACT_STRING(_refname);

    int retval = git_revwalk_hide_ref(revwalk, refname);
    free(refname);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}

EGIT_DOC(revwalk_push, "REVWALK OID", "Add OID as a new root for traversal in REVWALK");
emacs_value egit_revwalk_push(emacs_env *env, emacs_value _revwalk, emacs_value _oid)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    EM_ASSERT_STRING(_oid);

    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    int retval = git_revwalk_push(revwalk, &oid);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}

EGIT_DOC(revwalk_push_glob, "REVWALK GLOB", "Push references matched by GLOB to REVWALK.");
emacs_value egit_revwalk_push_glob(emacs_env *env, emacs_value _revwalk, emacs_value _glob)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    EM_ASSERT_STRING(_glob);

    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    char *glob = EM_EXTRACT_STRING(_glob);

    int retval = git_revwalk_push_glob(revwalk, glob);
    free(glob);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}

EGIT_DOC(revwalk_push_head, "REVWALK", "Push the repository HEAD to REVWALK.");
emacs_value egit_revwalk_push_head(emacs_env *env, emacs_value _revwalk)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    int retval = git_revwalk_push_head(revwalk);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}

EGIT_DOC(revwalk_push_range, "REVWALK RANGE",
         "Push and hide the endpoints of RANGE to REVWALK.\n"
         "The range should be of the form \"COMMITTISH..COMMITTISH\",\n"
         "The left-hand COMMITTISH will be hidden and the other pushed.");
emacs_value egit_revwalk_push_range(emacs_env *env, emacs_value _revwalk, emacs_value _range)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    EM_ASSERT_STRING(_range);

    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    char *range = EM_EXTRACT_STRING(_range);

    int retval = git_revwalk_push_range(revwalk, range);
    free(range);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}

EGIT_DOC(revwalk_push_ref, "REVWALK REFNAME", "Push the OID pointed to by REFNAME to REVWALK.");
emacs_value egit_revwalk_push_ref(emacs_env *env, emacs_value _revwalk, emacs_value _refname)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    EM_ASSERT_STRING(_refname);

    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    char *refname = EM_EXTRACT_STRING(_refname);

    int retval = git_revwalk_push_ref(revwalk, refname);
    free(refname);
    EGIT_CHECK_ERROR(retval);
    return em_nil;
}


// =============================================================================
// Setters and other mutating functions

EGIT_DOC(revwalk_reset, "REVWALK", "Reset REVWALK.");
emacs_value egit_revwalk_reset(emacs_env *env, emacs_value _revwalk)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    git_revwalk_reset(revwalk);
    return em_nil;
}

EGIT_DOC(revwalk_simplify_first_parent, "REVWALK",
         "No parents other than the first for each commit will be enqueued.");
emacs_value egit_revwalk_simplify_first_parent(emacs_env *env, emacs_value _revwalk)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);
    git_revwalk_simplify_first_parent(revwalk);
    return em_nil;
}

EGIT_DOC(revwalk_sorting, "REVWALK &optional MODE",
         "MODE is a list containing any combination of the symbols\n"
         "`topological', `time' and `reverse'.");
emacs_value egit_revwalk_sorting(emacs_env *env, emacs_value _revwalk, emacs_value _mode)
{
    EGIT_ASSERT_REVWALK(_revwalk);
    git_revwalk *revwalk = EGIT_EXTRACT(_revwalk);

    git_sort_t mode = GIT_SORT_NONE;
    {
        EM_DOLIST(car, _mode, list);

        if (EM_EQ(car, em_topological))
            mode |= GIT_SORT_TOPOLOGICAL;
        else if (EM_EQ(car, em_time))
            mode |= GIT_SORT_TIME;
        else if (EM_EQ(car, em_reverse))
            mode |= GIT_SORT_REVERSE;
        else {
            em_signal_wrong_value(env, car);
            return em_nil;
        }

        EM_DOLIST_END(list);
    }

    git_revwalk_sorting(revwalk, mode);
    return em_nil;
}
