#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-tree.h"


// =============================================================================
// Helpers

static emacs_value entry_to_emacs(emacs_env *env, const git_tree_entry *entry)
{
    git_filemode_t mode = git_tree_entry_filemode(entry);
    git_otype type = git_tree_entry_type(entry);
    const git_oid *oid = git_tree_entry_id(entry);
    const char *oid_s = git_oid_tostr_s(oid);
    const char *name = git_tree_entry_name(entry);

    emacs_value list_args[4];

    list_args[0] = em_nil;
    switch (mode) {
    case GIT_FILEMODE_UNREADABLE: list_args[0] = em_unreadable; break;
    case GIT_FILEMODE_TREE: list_args[0] = em_tree; break;
    case GIT_FILEMODE_BLOB: list_args[0] = em_blob; break;
    case GIT_FILEMODE_BLOB_EXECUTABLE: list_args[0] = em_blob_executable; break;
    case GIT_FILEMODE_LINK: list_args[0] = em_link; break;
    case GIT_FILEMODE_COMMIT: list_args[0] = em_commit; break;
    default: break;
    }

    list_args[1] = em_nil;
    switch (type) {
    case GIT_OBJ_COMMIT: list_args[1] = em_commit; break;
    case GIT_OBJ_TREE: list_args[1] = em_tree; break;
    case GIT_OBJ_BLOB: list_args[1] = em_blob; break;
    case GIT_OBJ_TAG: list_args[1] = em_tag; break;  // Probably impossible, but why assume
    default: break;
    }

    list_args[2] = env->make_string(env, oid_s, strlen(oid_s));
    list_args[3] = env->make_string(env, name, strlen(name));

    return em_list(env, list_args, 4);
}


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

EGIT_DOC(tree_entry_byid, "TREE ID",
         "Retrieve an entry from TREE by ID.\n"
         "See `tree-entry-byindex' for a description of the return value.");
emacs_value egit_tree_entry_byid(emacs_env *env, emacs_value _tree, emacs_value _oid)
{
    EGIT_ASSERT_TREE(_tree);
    EGIT_ASSERT_STRING(_oid);
    git_tree *tree = EGIT_EXTRACT(_tree);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);
    const git_tree_entry *entry = git_tree_entry_byid(tree, &oid);
    if (!entry)
        return em_nil; // TODO: Should we signal an error instead?
    return entry_to_emacs(env, entry);
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
    EGIT_ASSERT_INTEGER(_index);
    git_tree *tree = EGIT_EXTRACT(_tree);
    intmax_t index = EGIT_EXTRACT_INTEGER(_index);
    const git_tree_entry *entry = git_tree_entry_byindex(tree, index);
    if (!entry) {
        em_signal_args_out_of_range(env, index);
        return em_nil;
    }
    return entry_to_emacs(env, entry);
}

EGIT_DOC(tree_entry_byname, "TREE FILENAME",
         "Retrieve an entry from TREE by FILENAME.\n"
         "See `tree-entry-byindex' for a description of the return value.");
emacs_value egit_tree_entry_byname(emacs_env *env, emacs_value _tree, emacs_value _name)
{
    EGIT_ASSERT_TREE(_tree);
    EGIT_ASSERT_STRING(_name);
    git_tree *tree = EGIT_EXTRACT(_tree);
    char *name = EGIT_EXTRACT_STRING(_name);
    const git_tree_entry *entry = git_tree_entry_byname(tree, name);
    free(name);
    if (!entry)
        return em_nil; // TODO: Should we signal an error instead?
    return entry_to_emacs(env, entry);
}

EGIT_DOC(tree_entry_bypath, "TREE PATH",
         "Retrieve an entry from TREE, or any of its subtrees, by PATH.\n"
         "See `tree-entry-byindex' for a description of the return value.");
emacs_value egit_tree_entry_bypath(emacs_env *env, emacs_value _tree, emacs_value _path)
{
    EGIT_ASSERT_TREE(_tree);
    EGIT_ASSERT_STRING(_path);
    git_tree *tree = EGIT_EXTRACT(_tree);
    char *path = EGIT_EXTRACT_STRING(_path);
    git_tree_entry *entry;
    int retval = git_tree_entry_bypath(&entry, tree, path);
    free(path);
    EGIT_CHECK_ERROR(retval);
    emacs_value ret = entry_to_emacs(env, entry);
    git_tree_entry_free(entry);
    return ret;
}

EGIT_DOC(tree_entrycount, "TREE", "Return the number of entries in TREE.");
emacs_value egit_tree_entrycount(emacs_env *env, emacs_value _tree)
{
    EGIT_ASSERT_TREE(_tree);
    git_tree *tree = EGIT_EXTRACT(_tree);
    size_t entries = git_tree_entrycount(tree);
    return env->make_integer(env, entries);
}

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