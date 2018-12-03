#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-graph.h"


EGIT_DOC(graph_ahead_behind, "REPO LOCAL-ID UPSTREAM-ID",
         "Return a cons cell (AHEAD . BEHIND).\n"
         "AHEAD is the number of unique commits in UPSTREAM-ID.\n"
         "BEHIND is the number of unique commits in LOCAL-ID.");
emacs_value egit_graph_ahead_behind(
    emacs_env *env, emacs_value _repo, emacs_value _local, emacs_value _upstream)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_local);
    EM_ASSERT_STRING(_upstream);

    git_oid local, upstream;
    EGIT_EXTRACT_OID(_local, local);
    EGIT_EXTRACT_OID(_upstream, upstream);
    git_repository *repo = EGIT_EXTRACT(_repo);

    size_t ahead, behind;
    int retval = git_graph_ahead_behind(&ahead, &behind, repo, &local, &upstream);
    EGIT_CHECK_ERROR(retval);

    return em_cons(env, EM_INTEGER(ahead), EM_INTEGER(behind));
}

EGIT_DOC(graph_descendant_p, "REPO COMMIT-ID ANCESTOR-ID",
         "Return non-nil if COMMIT-ID is a descendant of ANCESTOR-ID.");
emacs_value egit_graph_descendant_p(
    emacs_env *env, emacs_value _repo, emacs_value _commit, emacs_value _ancestor)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_commit);
    EM_ASSERT_STRING(_ancestor);

    git_oid commit, ancestor;
    EGIT_EXTRACT_OID(_commit, commit);
    EGIT_EXTRACT_OID(_ancestor, ancestor);
    git_repository *repo = EGIT_EXTRACT(_repo);

    int retval = git_graph_descendant_of(repo, &commit, &ancestor);
    EGIT_CHECK_ERROR(retval);
    return retval ? esym_t : esym_nil;
}
