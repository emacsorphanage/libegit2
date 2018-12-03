#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
#include "interface.h"
#include "egit-tree.h"


// =============================================================================
// Constructors

EGIT_DOC(tree_lookup, "REPO OID", "Look up a tree in REPO by OID.");
emacs_value egit_tree_lookup(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    git_tree *tree;
    int retval = git_tree_lookup(&tree, repo, &oid);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_TREE, tree, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(tree_lookup_prefix, "REPO OID", "Lookup a tree in REPO by shortened OID.");
emacs_value egit_tree_lookup_prefix(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    size_t len;
    EGIT_EXTRACT_OID_PREFIX(_oid, oid, len);

    git_tree *tree;
    int retval = git_tree_lookup_prefix(&tree, repo, &oid, len);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_TREE, tree, EM_EXTRACT_USER_PTR(_repo));
}


// =============================================================================
// Getters

EGIT_DOC(tree_entry_byid, "TREE ID",
         "Retrieve an entry from TREE by ID.\n"
         "See `tree-entry-byindex' for a description of the return value.");
emacs_value egit_tree_entry_byid(emacs_env *env, emacs_value _tree, emacs_value _oid)
{
    EGIT_ASSERT_TREE(_tree);
    EM_ASSERT_STRING(_oid);
    git_tree *tree = EGIT_EXTRACT(_tree);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);
    const git_tree_entry *entry = git_tree_entry_byid(tree, &oid);
    if (!entry)
        return esym_nil; // TODO: Should we signal an error instead?
    return egit_tree_entry_to_emacs(env, entry);
}

EGIT_DOC(tree_entry_byindex, "TREE N",
         "Return the Nth entry in TREE.\n"
         "The return value is a list of four elements:\n"
         "  (MODE TYPE ID FILENAME)\n\n"
         "MODE is a symbol describing the file mode of the entry. It is one of\n"
         "- `unreadable'\n"
         "- `tree'\n"
         "- `blob'\n"
         "- `blob-executable'\n"
         "- `link'\n"
         "- `commit'\n\n"
         "TYPE is a symbol describing the type of object. It is one of\n"
         "- `commit'\n"
         "- `tree'\n"
         "- `blob'\n"
         "- `tag'\n\n"
         "ID is the object ID, and FILENAME is the relative path.");
emacs_value egit_tree_entry_byindex(emacs_env *env, emacs_value _tree, emacs_value _index)
{
    EGIT_ASSERT_TREE(_tree);
    EM_ASSERT_INTEGER(_index);
    git_tree *tree = EGIT_EXTRACT(_tree);
    intmax_t index = EM_EXTRACT_INTEGER(_index);
    const git_tree_entry *entry = git_tree_entry_byindex(tree, index);
    if (!entry) {
        em_signal_args_out_of_range(env, index);
        return esym_nil;
    }
    return egit_tree_entry_to_emacs(env, entry);
}

EGIT_DOC(tree_entry_byname, "TREE FILENAME",
         "Retrieve an entry from TREE by FILENAME.\n"
         "See `tree-entry-byindex' for a description of the return value.");
emacs_value egit_tree_entry_byname(emacs_env *env, emacs_value _tree, emacs_value _name)
{
    EGIT_ASSERT_TREE(_tree);
    EM_ASSERT_STRING(_name);
    git_tree *tree = EGIT_EXTRACT(_tree);
    char *name = EM_EXTRACT_STRING(_name);
    const git_tree_entry *entry = git_tree_entry_byname(tree, name);
    free(name);
    if (!entry)
        return esym_nil; // TODO: Should we signal an error instead?
    return egit_tree_entry_to_emacs(env, entry);
}

EGIT_DOC(tree_entry_bypath, "TREE PATH",
         "Retrieve an entry from TREE, or any of its subtrees, by PATH.\n"
         "See `tree-entry-byindex' for a description of the return value.");
emacs_value egit_tree_entry_bypath(emacs_env *env, emacs_value _tree, emacs_value _path)
{
    EGIT_ASSERT_TREE(_tree);
    EM_ASSERT_STRING(_path);
    git_tree *tree = EGIT_EXTRACT(_tree);
    char *path = EM_EXTRACT_STRING(_path);
    git_tree_entry *entry;
    int retval = git_tree_entry_bypath(&entry, tree, path);
    free(path);
    EGIT_CHECK_ERROR(retval);
    emacs_value ret = egit_tree_entry_to_emacs(env, entry);
    git_tree_entry_free(entry);
    return ret;
}

EGIT_DOC(tree_entrycount, "TREE", "Return the number of entries in TREE.");
emacs_value egit_tree_entrycount(emacs_env *env, emacs_value _tree)
{
    EGIT_ASSERT_TREE(_tree);
    git_tree *tree = EGIT_EXTRACT(_tree);
    size_t entries = git_tree_entrycount(tree);
    return EM_INTEGER(entries);
}

EGIT_DOC(tree_id, "TREE", "Return the ID of TREE.");
emacs_value egit_tree_id(emacs_env *env, emacs_value _tree)
{
    EGIT_ASSERT_TREE(_tree);
    git_tree *tree = EGIT_EXTRACT(_tree);
    const git_oid *oid = git_tree_id(tree);
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(tree_owner, "TREE", "Return the repository that TREE belongs to.");
emacs_value egit_tree_owner(emacs_env *env, emacs_value _tree)
{
    EGIT_ASSERT_TREE(_tree);
    egit_object *owner = EGIT_EXTRACT_PARENT(_tree);
    owner->refcount++;
    return EM_USER_PTR(owner, egit_finalize);
}


// =============================================================================
// Tree walk

static int tree_walk_callback(const char *root, const git_tree_entry *entry, void *payload)
{
    egit_generic_payload *ctx = (egit_generic_payload*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[2];
    args[0] = EM_STRING(root);
    args[1] = egit_tree_entry_to_emacs(env, entry);
    emacs_value ret = env->funcall(env, ctx->func, 2, args);

    EM_RETURN_IF_NLE(GIT_EUSER);
    return EM_EQ(ret, esym_skip) ? 1 : 0;
}

EGIT_DOC(tree_walk, "TREE ORDER FUNCTION",
         "Walk TREE and its subtrees and call FUNCTION for each entry.\n"
         "ORDER may be either `pre' or `post', giving the tree travelsar order.\n\n"
         "FUNCTION is called with two arguments: PATH and ENTRY.\n"
         "  - PATH is the path relative to the repository root\n"
         "  - ENTRY is a tree entry (see `libgit-tree-entry-byindex')\n\n"
         "If FUNCTION returns the symbol `skip' (in pre-order traversal),\n"
         "the passed entry will be skipped on the traversal.");
emacs_value egit_tree_walk(emacs_env *env, emacs_value _tree, emacs_value order, emacs_value function)
{
    EGIT_ASSERT_TREE(_tree);
    EM_ASSERT_FUNCTION(function);

    git_treewalk_mode mode;
    if (EM_EQ(order, esym_pre))
        mode = GIT_TREEWALK_PRE;
    else if (EM_EQ(order, esym_post))
        mode = GIT_TREEWALK_POST;
    else {
        em_signal_wrong_value(env, order);
        return esym_nil;
    }

    git_tree *tree = EGIT_EXTRACT(_tree);
    egit_generic_payload ctx = {.env = env, .func = function};

    int retval = git_tree_walk(tree, mode, &tree_walk_callback, (void*)(&ctx));

    if (retval != GIT_EUSER)
        EGIT_CHECK_ERROR(retval);

    return esym_nil;
}
