#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-tree.h"


// =============================================================================
// Getters

EGIT_DOC(tree_id, "TREE", "Return the ID of TREE.");
emacs_value egit_tree_id(emacs_env *env, emacs_value _tree)
{
    EGIT_ASSERT_TREE(_tree);
    git_commit *tree = EGIT_EXTRACT(_tree);
    const git_oid *oid = git_tree_id(tree);
    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}
