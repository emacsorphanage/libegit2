#include <limits.h>
#include <string.h>
#include "git2.h"

#include "egit-options.h"
#include "egit-repository.h"
#include "egit.h"
#include "interface.h"

#include "egit-revert.h"

EGIT_DOC(revert, "REPO COMMIT &optional MERGE-OPTIONS CHECKOUT-OPTIONS MAINLINE",
         "Reverts the given COMMIT, producing changes in the index and "
         "working directory.\n"
         "For MERGE-OPTIONS, see `libgit-merge'\n"
         "For CHECKOUT-OPTIONS, see `libgit-checkout-head'.\n"
         "For merge commits, the MAINLINE is treated as the parent.");
emacs_value egit_revert(emacs_env *env, emacs_value _repo, emacs_value _commit,
                        emacs_value _merge_opts, emacs_value _checkout_opts,
                        emacs_value _mainline)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_COMMIT(_commit);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_commit *commit = EGIT_EXTRACT(_commit);

    git_revert_options opts;
    int retval = git_revert_init_options(&opts, GIT_REVERT_OPTIONS_VERSION);
    EGIT_CHECK_ERROR(retval);

    egit_merge_options_parse(env, _merge_opts, &opts.merge_opts);
    EM_RETURN_NIL_IF_NLE();

    egit_checkout_options_parse(env, _checkout_opts, &opts.checkout_opts);
    EM_RETURN_NIL_IF_NLE();

    intmax_t mainline = EM_EXTRACT_INTEGER_OR_DEFAULT(_mainline, 0);
    if (mainline < 0 || mainline > INT_MAX) {
        em_signal_wrong_value(env, _mainline);
        return esym_nil;
    }
    opts.mainline = (unsigned int) mainline;

    retval = git_revert(repo, commit, &opts);
    egit_checkout_options_release(&opts.checkout_opts);
    EGIT_CHECK_ERROR(retval);

    return esym_t;
}
