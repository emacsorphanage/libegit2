#include <limits.h>
#include <string.h>
#include "git2.h"

#include "egit-options.h"
#include "egit-repository.h"
#include "egit.h"
#include "interface.h"

#include "egit-cherrypick.h"

EGIT_DOC(cherrypick, "REPO COMMIT &optional MERGE-OPTIONS CHECKOUT-OPTIONS MAINLINE",
         "Cherry-pick the given COMMIT, producing changes in the index and "
         "working directory.\n"
         "For MERGE-OPTIONS, see `libgit-merge'\n"
         "For CHECKOUT-OPTIONS, see `libgit-checkout-head'.\n"
         "For merge commits, the MAINLINE is treated as the parent.");
emacs_value egit_cherrypick(emacs_env *env,
                            emacs_value _repo,
                            emacs_value _commit,
                            emacs_value _merge_opts,
                            emacs_value _checkout_opts,
                            emacs_value _mainline)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_COMMIT(_commit);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_commit *commit = EGIT_EXTRACT(_commit);

    git_cherrypick_options opts;
    int retval = git_cherrypick_init_options(&opts, GIT_REVERT_OPTIONS_VERSION);
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

    retval = git_cherrypick(repo, commit, &opts);
    egit_checkout_options_release(&opts.checkout_opts);
    EGIT_CHECK_ERROR(retval);

    return esym_t;
}

EGIT_DOC(cherrypick_commit, "REPO CHERRYPICK-COMMIT OUR-COMMIT &optional MERGE-OPTIONS MAINLINE",
         "Cherry-pick the given CHERRYPICK-COMMIT against the given OUR-COMMIT commit, "
         "returns an index object that reflects the result of the revert.\n"
         "For MERGE-OPTIONS, see `libgit-merge'.\n"
         "For merge commits, the MAINLINE is treated as the parent.");
emacs_value egit_cherrypick_commit(emacs_env *env, emacs_value _repo,
                                   emacs_value _cherrypick_commit,
                                   emacs_value _our_commit,
                                   emacs_value _merge_opts,
                                   emacs_value _mainline)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_COMMIT(_cherrypick_commit);
    EGIT_ASSERT_COMMIT(_our_commit);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_commit *cherrypick_commit = EGIT_EXTRACT(_cherrypick_commit);
    git_commit *our_commit = EGIT_EXTRACT(_our_commit);

    git_merge_options merge_opts;
    egit_merge_options_parse(env, _merge_opts, &merge_opts);
    EM_RETURN_NIL_IF_NLE();

    intmax_t mainline = EM_EXTRACT_INTEGER_OR_DEFAULT(_mainline, 0);
    if (mainline < 0 || mainline > INT_MAX) {
        em_signal_wrong_value(env, _mainline);
        return esym_nil;
    }

    git_index *index = NULL;
    int retval = git_cherrypick_commit(&index, repo,
                                       cherrypick_commit, our_commit,
                                       (unsigned int) mainline, &merge_opts);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_INDEX, index, EM_EXTRACT_USER_PTR(_repo));
}
