#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-options.h"
#include "egit-util.h"
#include "interface.h"
#include "egit-submodule.h"


// =============================================================================
// Helpers - status values

static emacs_value status_decode(emacs_env *env, emacs_value flag, unsigned int status)
{
    if (EM_EXTRACT_BOOLEAN(flag)) {
        emacs_value retval;
        em_checkflag_submodule_status(&retval, env, flag, status, true);
        return retval;
    }

    return em_getlist_submodule_status(env, status);
}


// =============================================================================
// Constructors

EGIT_DOC(submodule_add_setup, "REPO URL PATH &optional LINKP",
         "Set up a new submodule for checkout.\n"
         "This emulates `git submodule add' up to fetch and checkout.\n"
         "If LINK is non-nil, PATH will contain a git link to a repo\n"
         "in REPO's own .git/modules.");
emacs_value egit_submodule_add_setup(
    emacs_env *env, emacs_value _repo, emacs_value _url,
    emacs_value _path, emacs_value linkp)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_url);
    EM_ASSERT_STRING(_path);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *url = EM_EXTRACT_STRING(_url);
    char *path = EM_EXTRACT_STRING(_path);
    git_submodule *sub;
    int retval = git_submodule_add_setup(&sub, repo, url, path, EM_EXTRACT_BOOLEAN(linkp));
    free(url);
    free(path);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_SUBMODULE, sub, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(submodule_lookup, "REPO NAME", "Look up a submodule in REPO by NAME or path.");
emacs_value egit_submodule_lookup(emacs_env *env, emacs_value _repo, emacs_value _name)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);
    git_submodule *sub;
    int retval = git_submodule_lookup(&sub, repo, name);
    free(name);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_SUBMODULE, sub, EM_EXTRACT_USER_PTR(_repo));
}


// =============================================================================
// Getters

EGIT_DOC(submodule_branch, "SUBMODULE", "Get the branch name for SUBMODULE.");
emacs_value egit_submodule_branch(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    const char *branch = git_submodule_branch(sub);
    if (!branch)
        return esym_nil;
    return EM_STRING(branch);
}

EGIT_DOC(submodule_fetch_recurse_submodules, "SUBMODULE",
         "Get the fetchRecurseSubmodules rule for SUBMODULE.\n"
         "This is `nil', `ondemand' or `t'.");
emacs_value egit_submodule_fetch_recurse_submodules(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    git_submodule_recurse_t rec = git_submodule_fetch_recurse_submodules(sub);
    return em_findenum_submodule_recurse(rec);
}

EGIT_DOC(submodule_head_id, "SUBMODULE", "Get the ID for SUBMODULE in HEAD.");
emacs_value egit_submodule_head_id(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    const git_oid *oid = git_submodule_head_id(sub);
    if (!oid)
        return esym_nil;
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(submodule_ignore, "SUBMODULE",
         "Get the ignore rule for SUBMODULE.\n"
         "This is one of the symbols `none', `untracked', `dirty' and `all'.");
emacs_value egit_submodule_ignore(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    git_submodule_ignore_t ignore = git_submodule_ignore(sub);
    return em_findenum_submodule_ignore(ignore);
}

EGIT_DOC(submodule_index_id, "SUBMODULE", "Get the ID for SUBMODULE in the index.");
emacs_value egit_submodule_index_id(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    const git_oid *oid = git_submodule_index_id(sub);
    if (!oid)
        return esym_nil;
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(submodule_location, "SUBMODULE &optional FLAG",
         "Get the location of SUBMODULE.\n"
         "This is a lightweight version of `libgit-submodule-status',\n"
         "only checking the first four status values: `in-head', `in-index',\n"
         "`in-config' and `in-wd'.\n\n"
         "See `libgit-submodule-status' for an explanation of FLAG.");
emacs_value egit_submodule_location(emacs_env *env, emacs_value _sub, emacs_value flag)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    unsigned int loc;
    int retval = git_submodule_location(&loc, sub);
    EGIT_CHECK_ERROR(retval);

    return status_decode(env, flag, loc);
}

EGIT_DOC(submodule_name, "SUBMODULE", "Get the name of SUBMODULE.");
emacs_value egit_submodule_name(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    const char *name = git_submodule_name(sub);
    return EM_STRING(name);
}

EGIT_DOC(submodule_open, "SUBMODULE", "Get the sub-repository associated with SUBMODULE.");
emacs_value egit_submodule_open(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    git_repository *repo;
    int retval = git_submodule_open(&repo, sub);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_REPOSITORY, repo, NULL);
}

EGIT_DOC(submodule_owner, "SUBMODULE", "Get the repository in which SUBMODULE lives.");
emacs_value egit_submodule_owner(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    egit_object *owner = EGIT_EXTRACT_PARENT(_sub);
    owner->refcount++;
    return EM_USER_PTR(owner, egit_finalize);
}

EGIT_DOC(submodule_path, "SUBMODULE", "Get the path of SUBMODULE.");
emacs_value egit_submodule_path(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    const char *path = git_submodule_path(sub);
    return EM_STRING(path);
}

EGIT_DOC(submodule_status, "REPO NAME &optional IGNORE FLAG",
         "Get a list of symbols describing the status of a submodule.\n"
         "REPO is the repository to search in, and NAME is the name of the submodule.\n"
         "IGNORE indicates the ignore rule to use, one of `none' (default), `untracked',\n"
         "`dirty' and `all'.\n\n"
         "The following are always returned.\n"
         "- `in-head': superproject head contains submodule\n"
         "- `in-index': superproject index contains submodule\n"
         "- `in-config': superproject gitmodules contains submodule\n"
         "- `in-wd': superproject workdir contains submodule\n\n"
         "The following are returned as long as IGNORE is not `all'.\n"
         "- `index-added': submodule is in index, not in head\n"
         "- `index-deleted': submodule is in head, not in index\n"
         "- `index-modified': index and head don't match\n"
         "- `wd-uninitialized': workdir contains empty directory\n"
         "- `wd-added': in workdir, but not index\n"
         "- `wd-deleted': in index, but not workdir\n"
         "- `wd-modified': index and workdir head don't match\n\n"
         "The following are only returned if IGNORE Is `none' or `untracked'.\n"
         "- `wd-index-modified': submodule workdir index is dirty\n"
         "- `wd-wd-modified': submodule workdir has modified files\n\n"
         "The following is only returned if the IGNORE is `none':\n"
         "- `wd-untracked': submodule workdir contains untracked files\n\n"
         "If the optional FLAG is non-nil, it may be any of the above symbols,\n"
         "in which case the return value is non-nil if that flag is present.\n"
         "In other words, these are equivalent:\n\n"
         "(libgit-submodule-status REPO NAME IGNORE FLAG)\n\n"
         "(memq FLAG (libgit-submodule-status REPO NAME IGNORE))");
emacs_value egit_submodule_status(
    emacs_env *env, emacs_value _repo, emacs_value _name,
    emacs_value _ignore, emacs_value flag)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);

    git_submodule_ignore_t ignore;
    if (!em_findsym_submodule_ignore(&ignore, env, _ignore, true))
        return esym_nil;

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);

    unsigned int status;
    int retval = git_submodule_status(&status, repo, name, ignore);
    free(name);
    EGIT_CHECK_ERROR(retval);

    return status_decode(env, flag, status);
}

EGIT_DOC(submodule_update_strategy, "SUBMODULE",
         "Get the update rule for SUBMODULE.\n"
         "This is one of the symbols `checkout', `rebase', `merge' and `none'.");
emacs_value egit_submodule_update_strategy(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    git_submodule_update_t update = git_submodule_update_strategy(sub);
    return em_findenum_submodule_update(update);
}

EGIT_DOC(submodule_url, "SUBMODULE", "Get the url of SUBMODULE.");
emacs_value egit_submodule_url(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    const char *url = git_submodule_url(sub);
    return EM_STRING(url);
}

EGIT_DOC(submodule_wd_id, "SUBMODULE", "Get the ID for SUBMODULE in the working directory.");
emacs_value egit_submodule_wd_id(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    const git_oid *oid = git_submodule_wd_id(sub);
    if (!oid)
        return esym_nil;
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}


// =============================================================================
// Foreach

static int submodule_callback(git_submodule *sub, const char *name, void *payload)
{
    egit_generic_payload *ctx = (egit_generic_payload*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[2];
    args[0] = egit_wrap(env, EGIT_SUBMODULE, sub, ctx->parent);
    args[1] = EM_STRING(name);
    env->funcall(env, ctx->func, 2, args);

    EM_RETURN_IF_NLE(GIT_EUSER);
    return 0;
}

EGIT_DOC(submodule_foreach, "REPO FUNC",
         "Call FUNC for each submodule in REPO.\n"
         "FUNC receives two arguments: the submodule object and its name.");
emacs_value egit_submodule_foreach(emacs_env *env, emacs_value _repo, emacs_value func)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_FUNCTION(func);

    egit_generic_payload ctx = {.env = env, .func = func, .parent = EM_EXTRACT_USER_PTR(_repo)};
    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval = git_submodule_foreach(repo, &submodule_callback, &ctx);

    EM_RETURN_NIL_IF_NLE();
    if (retval == GIT_EUSER)
        return esym_nil;
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}


// =============================================================================
// Operations

EGIT_DOC(submodule_add_finalize, "SUBMODULE",
         "Resolve the setup of SUBMODULE.\n"
         "This should be called after `libgit-add-setup', and after clone.");
emacs_value egit_submodule_add_finalize(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    int retval = git_submodule_add_finalize(sub);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(submodule_add_to_index, "SUBMODULE WRITE",
         "Add HEAD of SUBMODULE to index in superproject.\n"
         "If WRITE is non-nil, the index will be immediately written.");
emacs_value egit_submodule_add_to_index(emacs_env *env, emacs_value _sub, emacs_value write)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    int retval = git_submodule_add_to_index(sub, EM_EXTRACT_BOOLEAN(write));
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(submodule_init, "SUBMODULE &optional FORCE",
         "Copy submodule info to .git/config.\n"
         "If FORCE is non-nil, overwrite even if an entry already exists.");
emacs_value egit_submodule_init(emacs_env *env, emacs_value _sub, emacs_value force)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    int retval = git_submodule_init(sub, EM_EXTRACT_BOOLEAN(force));
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(submodule_reload, "SUBMODULE &optional FORCE",
         "Re-read info about SUBMODULE from config, index and HEAD.\n"
         "If FORCE is non-nil, force reload even if data does not seem\n"
         "outdated.");
emacs_value egit_submodule_reload(emacs_env *env, emacs_value _sub, emacs_value force)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    int retval = git_submodule_reload(sub, EM_EXTRACT_BOOLEAN(force));
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(submodule_repo_init, "SUBMODULE &optional LINKP",
         "Set up and return the subrepo for SUBMODULE in preparation for clone.\n"
         "If LINK is non-nil, the subrepo will contain a git link to a repo\n"
         "in the parent repository's own .git/modules.");
emacs_value egit_submodule_repo_init(emacs_env *env, emacs_value _sub, emacs_value linkp)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    git_repository *repo;
    int retval = git_submodule_repo_init(&repo, sub, EM_EXTRACT_BOOLEAN(linkp));
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_REPOSITORY, repo, NULL);
}

EGIT_DOC(submodule_set_branch, "REPO NAME BRANCHNAME",
         "Set the branch of submodule NAME to BRANCHNAME.\n"
         "After this, you may wish to call `libgit-submodule-sync'.");
emacs_value egit_submodule_set_branch(
    emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _refname)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_refname);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);
    char *refname = EM_EXTRACT_STRING(_refname);
    int retval = git_submodule_set_branch(repo, name, refname);
    free(name);
    free(refname);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(submodule_set_fetch_recurse_submodules, "REPO NAME &optional VALUE",
         "Set the fetchRecurseSubmodules rule for submodule NAME.\n"
         "Possible VALUE are nil, `ondemand', or other non-nil.");
emacs_value egit_submodule_set_fetch_recurse_submodules(
    emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _value)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);

    git_submodule_recurse_t value;
    if (!em_findsym_submodule_recurse(&value, env, _value, false))
        value = GIT_SUBMODULE_RECURSE_YES;

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);
    int retval = git_submodule_set_fetch_recurse_submodules(repo, name, value);
    free(name);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(submodule_set_ignore, "REPO NAME VALUE",
         "Set the ignore rule for submodule NAME.\n"
         "Possible VALUE are `none', `dirty', `untracked' and `all'.");
emacs_value egit_submodule_set_ignore(
    emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _value)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);

    git_submodule_ignore_t value;
    if (!em_findsym_submodule_ignore(&value, env, _value, true))
        return esym_nil;

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);
    int retval = git_submodule_set_ignore(repo, name, value);
    free(name);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(submodule_set_update, "REPO NAME VALUE",
         "Set the ignore rule for submodule NAME.\n"
         "Possible VALUE are `checkout', `rebase', `merge' and `none'.");
emacs_value egit_submodule_set_update(
    emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _value)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);

    git_submodule_update_t value;
    if (!em_findsym_submodule_update(&value, env, _value, true))
        return esym_nil;

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);
    int retval = git_submodule_set_update(repo, name, value);
    free(name);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(submodule_set_url, "REPO NAME URL",
         "Set the URL of submodule NAME to URL.\n"
         "After this, you may wish to call `libgit-submodule-sync'.");
emacs_value egit_submodule_set_url(
    emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _url)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_url);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);
    char *url = EM_EXTRACT_STRING(_url);
    int retval = git_submodule_set_url(repo, name, url);
    free(name);
    free(url);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(submodule_sync, "SUBMODULE", "Copy SUBMODULE's remote info into its repository.");
emacs_value egit_submodule_sync(emacs_env *env, emacs_value _sub)
{
    EGIT_ASSERT_SUBMODULE(_sub);
    git_submodule *sub = EGIT_EXTRACT(_sub);
    int retval = git_submodule_sync(sub);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(submodule_update, "SUBMODULE &optional INITP FETCHP CHECKOUT-OPTS FETCH-OPTS",
         "Update a submodule and checkout the subrepo to the commit\n"
         "specified in the index of the parent repo.\n"
         "If the submodule is missing it will be cloned.\n"
         "If INITP is non-nil, initialize the submodule if not already done.\n"
         "If FETCHP is non-nil, fetch from the remote if the subrepo does not\n"
         "contain the desired commit.\n"
         "For CHECKOUT-OPTS, see `libgit-checkout-head', and for FETCH-OPTS,\n"
         "see `libgit-remote-fetch'.");
emacs_value egit_submodule_update(
    emacs_env *env, emacs_value _sub, emacs_value initp, emacs_value fetchp,
    emacs_value checkout_opts, emacs_value fetch_opts)
{
    EGIT_ASSERT_SUBMODULE(_sub);

    git_submodule_update_options opts;
    opts.allow_fetch = EM_EXTRACT_BOOLEAN(fetchp);

    egit_checkout_options_parse(env, checkout_opts, &opts.checkout_opts);
    EM_RETURN_NIL_IF_NLE();

    egit_fetch_options_parse(env, fetch_opts, &opts.fetch_opts);
    EM_RETURN_NIL_IF_NLE();

    git_submodule *sub = EGIT_EXTRACT(_sub);
    int retval = git_submodule_update(sub, EM_EXTRACT_BOOLEAN(initp), &opts);
    egit_checkout_options_release(&opts.checkout_opts);
    egit_fetch_options_release(&opts.fetch_opts);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}
