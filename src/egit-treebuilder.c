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
        return esym_nil;
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
    return esym_nil;
}

EGIT_DOC(treebuilder_insert, "BUILDER PATH ID MODE",
         "Add a new entry to BUILDER.\n"
         "PATH is the relative path to the file, and ID is the object ID.\n"
         "MODE may be any of `tree', `blob', `blob-executable', `link', or `commit'.");
emacs_value egit_treebuilder_insert(
    emacs_env *env, emacs_value _builder, emacs_value _path,
    emacs_value _oid, emacs_value _mode)
{
    EGIT_ASSERT_TREEBUILDER(_builder);
    EM_ASSERT_STRING(_path);
    EM_ASSERT_STRING(_oid);

    git_filemode_t mode;
    if (!em_findsym_filemode(&mode, env, _mode, true))
        return esym_nil;

    git_treebuilder *bld = EGIT_EXTRACT(_builder);
    char *path = EM_EXTRACT_STRING(_path);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    int retval = git_treebuilder_insert(NULL, bld, path, &oid, mode);
    free(path);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(treebuilder_remove, "BUILDER PATH",
         "Delete the entry in BUILDER associated with PATH.");
emacs_value egit_treebuilder_remove(emacs_env *env, emacs_value _builder, emacs_value _path)
{
    EGIT_ASSERT_TREEBUILDER(_builder);
    EM_ASSERT_STRING(_path);

    git_treebuilder *bld = EGIT_EXTRACT(_builder);
    char *path = EM_EXTRACT_STRING(_path);
    int retval = git_treebuilder_remove(bld, path);
    free(path);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(treebuilder_write, "BUILDER",
         "Write the contents of BUILDER to a tree and return its ID.");
emacs_value egit_treebuilder_write(emacs_env *env, emacs_value _builder)
{
    EGIT_ASSERT_TREEBUILDER(_builder);
    git_treebuilder *bld = EGIT_EXTRACT(_builder);
    git_oid oid;
    int retval = git_treebuilder_write(&oid, bld);
    EGIT_CHECK_ERROR(retval);

    const char *oid_s = git_oid_tostr_s(&oid);
    return EM_STRING(oid_s);
}


// =============================================================================
// Filter

static int filter_callback(const git_tree_entry *entry, void *payload)
{
    egit_generic_payload *ctx = (egit_generic_payload*) payload;
    emacs_env *env = ctx->env;

    emacs_value arg = egit_tree_entry_to_emacs(env, entry);
    emacs_value retval = env->funcall(env, ctx->func, 1, &arg);

    // Ignore errors
    if (env->non_local_exit_check(env)) {
        env->non_local_exit_clear(env);
        return 0;
    }

    // Treat the elisp semantics as truthy value = keep,
    // as is common with filters
    return !EM_EXTRACT_BOOLEAN(retval);
}

EGIT_DOC(treebuilder_filter, "BUILDER FUNC",
         "Call FUNC on each entry of BUILDER.\n"
         "Those entries for which FUNC returns nil will be removed.\n"
         "See `libgit-tree-entry-byindex' for more information.\n"
         "Signals thrown in FUNC will be ignored.");
emacs_value egit_treebuilder_filter(emacs_env *env, emacs_value _builder, emacs_value func)
{
    EGIT_ASSERT_TREEBUILDER(_builder);
    EM_ASSERT_FUNCTION(func);

    egit_generic_payload payload = {.env = env, .func = func, .parent = NULL};
    git_treebuilder *bld = EGIT_EXTRACT(_builder);
    git_treebuilder_filter(bld, &filter_callback, &payload);
    return esym_nil;
}
