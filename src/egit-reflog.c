#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-reflog.h"


// =============================================================================
// Constructors

EGIT_DOC(reflog_read, "REPO REFNAME", "Open the reflog for REFNAME in REPO.");
emacs_value egit_reflog_read(emacs_env *env, emacs_value _repo, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *refname = EM_EXTRACT_STRING(_refname);
    git_reflog *reflog;
    int retval = git_reflog_read(&reflog, repo, refname);
    free(refname);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REFLOG, reflog, NULL);
}


// =============================================================================
// Getters

EGIT_DOC(reflog_entrycount, "REFLOG", "Get the number of entries in REFLOG");
emacs_value egit_reflog_entrycount(emacs_env *env, emacs_value _reflog)
{
    EGIT_ASSERT_REFLOG(_reflog);
    git_reflog *reflog = EGIT_EXTRACT(_reflog);
    return EM_INTEGER(git_reflog_entrycount(reflog));
}
