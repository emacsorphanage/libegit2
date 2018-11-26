#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-options.h"
#include "interface.h"
#include "egit-merge.h"


EGIT_DOC(merge_analysis, "REPO HEADS",
         "Analyze the effects of merging HEADS into the current HEAD of repo.\n"
         "HEADS should be a list of annotated commits.\n"
         "The return value is a cons (ANALYSIS . PREFERENCE) where ANALYSIS is\n"
         "a list with the following symbols:\n"
         "- `normal': a normal merge is possible\n"
         "- `up-to-date': inputs are reachable from HEAD, no merge necessary\n"
         "- `fastforward': a fast-forward merge is possible\n"
         "- `unborn': HEAD is unborn, no merge possible\n\n"
         "PREFERENCE indicates the merge.ff setting, and can be either `nil',\n"
         "`no-fastforward' or `fastforward-only'.");
emacs_value egit_merge_analysis(emacs_env *env, emacs_value _repo, emacs_value _heads)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    ptrdiff_t i = 0;
    ptrdiff_t nheads = egit_assert_list(env, EGIT_ANNOTATED_COMMIT, em_libgit_annotated_commit_p, _heads);
    if (nheads < 0)
        return em_nil;
    const git_annotated_commit *heads[nheads];
    {
        EM_DOLIST(h, _heads, get_heads);
        heads[i++] = EGIT_EXTRACT(h);
        EM_DOLIST_END(get_heads);
    }

    git_merge_analysis_t analysis;
    git_merge_preference_t preference;
    int retval = git_merge_analysis(&analysis, &preference, repo, heads, nheads);
    EGIT_CHECK_ERROR(retval);

    emacs_value _analysis[4];

    ptrdiff_t nanal = 0;
    if (analysis & GIT_MERGE_ANALYSIS_NORMAL)
        _analysis[nanal++] = em_normal;
    if (analysis & GIT_MERGE_ANALYSIS_UP_TO_DATE)
        _analysis[nanal++] = em_up_to_date;
    if (analysis & GIT_MERGE_ANALYSIS_FASTFORWARD)
        _analysis[nanal++] = em_fastforward;
    if (analysis & GIT_MERGE_ANALYSIS_UNBORN)
        _analysis[nanal++] = em_unborn;

    // These are bit flags but only one can be set
    emacs_value _preference = em_nil;
    if (preference & GIT_MERGE_PREFERENCE_NO_FASTFORWARD)
        _preference = em_no_fastforward;
    if (preference & GIT_MERGE_PREFERENCE_FASTFORWARD_ONLY)
        _preference = em_fastforward_only;

    return em_cons(env, em_list(env, _analysis, nanal), _preference);
}
