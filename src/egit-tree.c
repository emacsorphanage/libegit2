#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-tree.h"


// =============================================================================
// Constructors

EGIT_DOC(tree_lookup, "REPO OID", "Look up a tree in REPO by OID.");
emacs_value egit_tree_lookup(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    git_tree *tree;
    int retval = git_tree_lookup(&tree, repo, &oid);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_TREE, tree);
}


// =============================================================================
// Getters

EGIT_DOC(tree_id, "TREE", "Return the ID of TREE.");
emacs_value egit_tree_id(emacs_env *env, emacs_value _tree)
{
    EGIT_ASSERT_TREE(_tree);
    git_tree *tree = EGIT_EXTRACT(_tree);
    const git_oid *oid = git_tree_id(tree);
    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}

EGIT_DOC(tree_owner, "TREE", "Return the repository that TREE belongs to.");
emacs_value egit_tree_owner(emacs_env *env, emacs_value _tree)
{
    EGIT_ASSERT_TREE(_tree);
    git_tree *tree = EGIT_EXTRACT(_tree);
    git_repository *repo = git_tree_owner(tree);
    return egit_wrap_repository(env, repo);
}
