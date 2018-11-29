#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-treebuilder.h"


// =============================================================================
// Constructors

EGIT_DOC(treebuilder_new, "REPO &optional TREE",
         "Create a new treebuilder in REPO.\n"
         "If TREE is non-nil, initialize the builder with its contents.");
emacs_value egit_treebuilder_new(emacs_env *env, emacs_value _repo, emacs_value _tree)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    if (EM_EXTRACT_BOOLEAN(_tree))
        EGIT_ASSERT_TREE(_tree);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_tree *tree = EGIT_EXTRACT_OR_NULL(_tree);
    git_treebuilder *bld;
    int retval = git_treebuilder_new(&bld, repo, tree);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_TREEBUILDER, bld, EM_EXTRACT_USER_PTR(_repo));
}
