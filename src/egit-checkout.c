#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
#include "egit-options.h"
#include "interface.h"
#include "egit-checkout.h"


// =============================================================================
// Checkout functions

#define PARSE_OPTIONS()                                         \
    do {                                                        \
        egit_checkout_options_parse(env, opts, &options);       \
        EM_RETURN_NIL_IF_NLE();                                 \
    } while (0)

#define CLEANUP()                                    \
    do {                                             \
        egit_checkout_options_release(&options);     \
        EM_RETURN_NIL_IF_NLE();                      \
        if (retval == GIT_EUSER)                     \
            return esym_nil;                         \
        EGIT_CHECK_ERROR(retval);                    \
        return esym_nil;                             \
    } while (0)

EGIT_DOC(checkout_head, "REPO &optional OPTIONS",
         "Update files in the working tree of REPO to match the content of HEAD.\n\n"
         "OPTIONS is an alist with the following keys:\n"
         "- `strategy': May be either `none' (a dry run that checks for conflicts\n"
         "     without making actual changes), `safe' (the default; only makes\n"
         "     modification that will not lose changes), and `force' (take any\n"
         "     action necessary to make the working directory match the target).\n"
         "- `notify-when': A list of conditions upon which to notify.  May contain\n"
         "     any of the symbols `conflict', `dirty' (files that do not need update,\n"
         "     but which do not match the baseline), `updated' (files that change)\n"
         "     `untracked' and `ignored', or the single symbol `all'.\n"
         "- `notify': Function to call when notifying.  Receives two arguments:\n"
         "     the reason for notifying (one of the symbols above) and the path.\n"
         "     All notifications are made before any changes are made, and the function\n"
         "     may choose to return the symbol `abort' to cancel the checkout."
         "- `progress': Function to call during checkout.  Receives three arguments:\n"
         "     the current path (or nil), the current step and total number of steps.\n"
         "     Note that error signals from this function will be ignored!"
         "- `baseline': A tree or index object representing the expected contents of\n"
         "     the working directory.  Mismatch will result in an error unless the\n"
         "     `force' strategy is used.");
emacs_value egit_checkout_head(emacs_env *env, emacs_value _repo, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    git_checkout_options options;
    PARSE_OPTIONS();
    int retval = git_checkout_head(repo, &options);
    CLEANUP();
}

EGIT_DOC(checkout_index, "REPO &optional INDEX OPTIONS",
         "Update files in the working tree of REPO to match the content of INDEX.\n"
         "See `libgit-checkout-head' for details on OPTIONS.");
emacs_value egit_checkout_index(emacs_env *env, emacs_value _repo, emacs_value _index, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);

    if (EM_EXTRACT_BOOLEAN(_index))
        EGIT_ASSERT_INDEX(_index);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_index *index = EGIT_EXTRACT_OR_NULL(_index);

    git_checkout_options options;
    PARSE_OPTIONS();
    int retval = git_checkout_index(repo, index, &options);
    CLEANUP();
}

EGIT_DOC(checkout_tree, "REPO &optional TREEISH OPTIONS",
         "Update files in the index and working tree of REPO to match the content of TREEISH.\n"
         "See `libgit-checkout-head' for details on OPTIONS.");
emacs_value egit_checkout_tree(emacs_env *env, emacs_value _repo, emacs_value _treeish, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);

    // TODO: Do we need treeish assertions enough to make a macro?
    // Do we need to define a libgit-treeish-p?
    // If so, should it peel tags to confirm that the target actually is a tree or a commit?
    if (EM_EXTRACT_BOOLEAN(_treeish)) {
        egit_type treeish_type = egit_get_type(env, _treeish);
        if (treeish_type != EGIT_COMMIT && treeish_type != EGIT_TREE && treeish_type != EGIT_TAG) {
            em_signal_wrong_type(env, esym_libgit_tree_p, _treeish);
            return esym_nil;
        }
    }

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_object *treeish = EGIT_EXTRACT_OR_NULL(_treeish);

    git_checkout_options options;
    PARSE_OPTIONS();
    int retval = git_checkout_tree(repo, treeish, &options);
    CLEANUP();
}

#undef PARSE_OPTIONS
#undef CLEANUP
