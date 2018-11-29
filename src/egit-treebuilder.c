#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
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


// =============================================================================
// Getters

EGIT_DOC(treebuilder_entrycount, "BUILDER", "Get the number of entries in BUILDER.");
emacs_value egit_treebuilder_entrycount(emacs_env *env, emacs_value _builder)
{
    EGIT_ASSERT_TREEBUILDER(_builder);
    git_treebuilder *bld = EGIT_EXTRACT(_builder);
    return EM_INTEGER(git_treebuilder_entrycount(bld));
}

EGIT_DOC(treebuilder_get, "BUILDER PATH",
         "Get the entry in BUILDER associated with PATH.\n"
         "See `libgit-tree-entry-byindex' for more information.");
emacs_value egit_treebuilder_get(emacs_env *env, emacs_value _builder, emacs_value _path)
{
    EGIT_ASSERT_TREEBUILDER(_builder);
    EM_ASSERT_STRING(_path);

    git_treebuilder *bld = EGIT_EXTRACT(_builder);
    char *path = EM_EXTRACT_STRING(_path);
    const git_tree_entry *entry = git_treebuilder_get(bld, path);
    free(path);

    if (!entry)
        return em_nil;
    return egit_tree_entry_to_emacs(env, entry);
}


// =============================================================================
// Operations

EGIT_DOC(treebuilder_clear, "BUILDER", "Clear all entries in BUILDER.");
emacs_value egit_treebuilder_clear(emacs_env *env, emacs_value _builder)
{
    EGIT_ASSERT_TREEBUILDER(_builder);
    git_treebuilder *bld = EGIT_EXTRACT(_builder);
    git_treebuilder_clear(bld);
    return em_nil;
}
