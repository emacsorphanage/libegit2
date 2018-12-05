#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-options.h"
#include "egit-util.h"
#include "interface.h"
#include "egit-remote.h"


// =============================================================================
// Constructors

EGIT_DOC(remote_create, "REPO NAME URL",
         "Create and return a new remote in REPO named NAME pointing to URL.\n"
         "The new remote will have the default fetch refspec.");
emacs_value egit_remote_create(emacs_env *env, emacs_value _repo, emacs_value _name, emacs_value _url)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_url);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);
    char *url = EM_EXTRACT_STRING(_url);

    git_remote *remote;
    int retval = git_remote_create(&remote, repo, name, url);
    free(name);
    free(url);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REMOTE, remote, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(remote_lookup, "REPO NAME", "Look up a remote in REPO by NAME.");
emacs_value egit_remote_lookup(emacs_env *env, emacs_value _repo, emacs_value _name)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);
    git_remote *remote;
    int retval = git_remote_lookup(&remote, repo, name);
    free(name);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REMOTE, remote, EM_EXTRACT_USER_PTR(_repo));
}


// =============================================================================
// Getters

EGIT_DOC(remote_autotag, "REMOTE",
         "Get the autotag setting for REMOTE.\n"
         "This is one of the symbols `auto', `none', `all' or nil.");
emacs_value egit_remote_autotag(emacs_env *env, emacs_value _remote)
{
    EGIT_ASSERT_REMOTE(_remote);
    git_remote *remote = EGIT_EXTRACT(_remote);
    git_remote_autotag_option_t ret = git_remote_autotag(remote);
    return em_findenum_remote_autotag_option(ret);
}

EGIT_DOC(remote_get_refspec, "REMOTE N", "Get the Nth refspec of REMOTE.");
emacs_value egit_remote_get_refspec(emacs_env *env, emacs_value _remote, emacs_value _index)
{
    EGIT_ASSERT_REMOTE(_remote);
    EM_ASSERT_INTEGER(_index);
    git_remote *remote = EGIT_EXTRACT(_remote);
    intmax_t index = EM_EXTRACT_INTEGER(_index);

    const git_refspec *refspec = git_remote_get_refspec(remote, index);
    if (!refspec) {
        em_signal_args_out_of_range(env, index);
        return esym_nil;
    }

    return egit_wrap(env, EGIT_REFSPEC, refspec, EM_EXTRACT_USER_PTR(_remote));
}

EGIT_DOC(remote_get_refspecs, "REMOTE &optional PUSH",
         "Get the list of the refspecs of REMOTE.\n"
         "If PUSH is non-nil get the push refspecs, else the fetch refspecs.");
emacs_value egit_remote_get_refspecs(emacs_env *env, emacs_value _remote, emacs_value _push)
{
    EGIT_ASSERT_REMOTE(_remote);
    git_remote *remote = EGIT_EXTRACT(_remote);

    git_strarray out = {NULL, 0};
    int retval;
    if (!EM_EXTRACT_BOOLEAN(_push))
        retval = git_remote_get_fetch_refspecs(&out, remote);
    else
        retval = git_remote_get_push_refspecs(&out, remote);
    EGIT_CHECK_ERROR(retval);

    EGIT_RET_STRARRAY(out);
}

EGIT_DOC(remote_name, "REMOTE", "Get the name of REMOTE, or nil for in-memory remotes.");
emacs_value egit_remote_name(emacs_env *env, emacs_value _remote)
{
    EGIT_ASSERT_REMOTE(_remote);
    git_remote *remote = EGIT_EXTRACT(_remote);
    const char *name = git_remote_name(remote);
    if (name)
        return EM_STRING(name);
    return esym_nil;
}

EGIT_DOC(remote_owner, "REMOTE", "Get the repository that REMOTE belongs to.");
emacs_value egit_remote_owner(emacs_env *env, emacs_value _remote)
{
    EGIT_ASSERT_REMOTE(_remote);
    egit_object *owner = EGIT_EXTRACT_PARENT(_remote);
    owner->refcount++;
    return EM_USER_PTR(owner, egit_finalize);
}

EGIT_DOC(remote_pushurl, "REMOTE", "Get the push URL of REMOTE, or nil if none set.");
emacs_value egit_remote_pushurl(emacs_env *env, emacs_value _remote)
{
    EGIT_ASSERT_REMOTE(_remote);
    git_remote *remote = EGIT_EXTRACT(_remote);
    const char *url = git_remote_pushurl(remote);
    if (url)
        return EM_STRING(url);
    return esym_nil;
}

EGIT_DOC(remote_refspec_count, "REMOTE", "Get the number of refspecs configured in REMOTE.");
emacs_value egit_remote_refspec_count(emacs_env *env, emacs_value _remote)
{
    EGIT_ASSERT_REMOTE(_remote);
    git_remote *remote = EGIT_EXTRACT(_remote);
    return EM_INTEGER(git_remote_refspec_count(remote));
}

EGIT_DOC(remote_url, "REMOTE", "Get the URL of REMOTE.");
emacs_value egit_remote_url(emacs_env *env, emacs_value _remote)
{
    EGIT_ASSERT_REMOTE(_remote);
    git_remote *remote = EGIT_EXTRACT(_remote);
    const char *url = git_remote_url(remote);
    return EM_STRING(url);
}


// =============================================================================
// Miscellaneous

EGIT_DOC(remote_list, "REPO", "Get a list of remote names in REPO.");
emacs_value egit_remote_list(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    git_strarray out = {NULL, 0};
    int retval = git_remote_list(&out, repo);
    EGIT_CHECK_ERROR(retval);

    EGIT_RET_STRARRAY(out);
}

EGIT_DOC(remote_valid_name_p, "NAME", "Return non-nil if NAME is a valid name for a remote.");
emacs_value egit_remote_valid_name_p(emacs_env *env, emacs_value _name)
{
    EM_ASSERT_STRING(_name);
    char *name = EM_EXTRACT_STRING(_name);
    int retval = git_remote_is_valid_name(name);
    free(name);
    return retval ? esym_t : esym_nil;
}


// =============================================================================
// Operations

EGIT_DOC(remote_add_refspec, "REPO NAME REFSPEC &optional PUSH",
         "Add a new REFSPEC to the remote named NAME in REPO.\n"
         "If PUSH is non-nil, add a push refspec, else a fetch refspec.");
emacs_value egit_remote_add_refspec(
    emacs_env *env, emacs_value _repo, emacs_value _name,
    emacs_value _refspec, emacs_value _push)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_refspec);

    bool push = EM_EXTRACT_BOOLEAN(_push);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);
    char *refspec = EM_EXTRACT_STRING(_refspec);

    int retval = push ? git_remote_add_push(repo, name, refspec) :
                 git_remote_add_fetch(repo, name, refspec);
    free(name);
    free(refspec);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(remote_fetch, "REMOTE &optional REFSPECS OPTIONS MESSAGE",
         "Fetch from REMOTE.\n"
         "If REFSPECS is not given, use the default fetch refspecs of REMOTE.\n"
         "MESSAGE (default 'fetch') will be inserted into the reflog.\n\n"
         "OPTIONS is an alist with the following allowed keys:\n"
         "- `callbacks': an alist of callback functions (see below)\n"
         "- `proxy': an alist of proxy settings (see below)\n"
         "- `headers': a list of extra headers to send\n"
         "- `download-tags': either nil, or any of the symbols `auto', `none' or `all'\n"
         "- `update-fetchhead': if non-nil (default), update FETCH_HEAD\n\n"
         "Callbacks is an alist where all the values are functions:\n"
         "- `certificate-check': called if certificate verification fails. If this function\n"
         "     signals an error, the fetch is aborted. This function receives three arguments:\n"
         "     1. The certificate. This is either nil (unknown type) or a list (`x509' DATA)\n"
         "        for an X.509 certificate, or a cons (`hostkey-libssh2' . PLIST), where\n"
         "        PLIST is a property list with certificate hash strings (possible keys are\n"
         "        `md5' and `sha1'.\n"
         "     2. `validp': non-nil if libgit2 thinks the certificate is valid.\n"
         "     3. `host': the hostname, if applicable.\n"
         "- `credentials': called if the remote requires authentication. This function receives\n"
         "     three arguments: the URL, the username derived from the URL, if applicable, and a\n"
         "     list of acceptable credential types: `userpass-plaintext', `ssh-key', `ssh-custom'\n"
         "     (not implemented), `default', `ssh-interactive' (not implemented), `username' and \n"
         "     `ssh-memory'. The function should return a credential object created with one of\n"
         "     `libgit-cred-...' functions.\n"
         "- `sideband-progress': receives one string argument with progress from the remote\n"
         "- `transfer-progress': called during download with the current progress. This function\n"
         "     receives seven arguments: total number of objects, number of indexed objects,\n"
         "     number of received objects, number of local objects, total number of deltas,\n"
         "     number of indexed deltas and bytes received.\n\n"
         "Proxy settings is an alist with the following keys:\n"
         "- `type': either nil (default: no proxy), `auto' or `specified'\n"
         "- `url': the proxy URL\n"
         "- `certificate-check': a certificate check callback (see above)\n"
         "- `credentials': a credential callback (see above)");
emacs_value egit_remote_fetch(
    emacs_env *env, emacs_value _remote, emacs_value _refspecs, emacs_value opts, emacs_value _msg)
{
    EGIT_ASSERT_REMOTE(_remote);
    EM_ASSERT_STRING_OR_NIL(_msg);

    git_strarray refspecs;
    if (!egit_strarray_from_list(&refspecs, env, _refspecs))
        return esym_nil;

    git_fetch_options options;
    egit_fetch_options_parse(env, opts, &options);
    if (env->non_local_exit_check(env)) {
        egit_strarray_dispose(&refspecs);
        return esym_nil;
    }

    git_remote *remote = EGIT_EXTRACT(_remote);
    char *msg = EM_EXTRACT_STRING_OR_NULL(_msg);
    int retval = git_remote_fetch(remote, &refspecs, &options, msg);

    free(msg);
    egit_strarray_dispose(&refspecs);
    egit_fetch_options_release(&options);

    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}

EGIT_DOC(remote_push, "REMOTE &optional REFSPECS OPTIONS",
         "Push to REMOTE.\n"
         "If REFSPECS is not given, use the default push refspecs of REMOTE.\n\n"
         "OPTIONS is an alist with the following allowed keys:\n"
         "- `callbacks': an alist of callback functions (see `libgit-remote-fetch')\n"
         "- `proxy': an alist of proxy settings (see `libgit-remote-fetch')\n"
         "- `headers': a list of extra headers to send\n"
         "- `threads': number of threads to use for creating the packfile\n"
         "     (default 1, use nil for auto-detection)");
emacs_value egit_remote_push(emacs_env *env, emacs_value _remote, emacs_value _refspecs, emacs_value opts)
{
    EGIT_ASSERT_REMOTE(_remote);

    git_strarray refspecs;
    if (!egit_strarray_from_list(&refspecs, env, _refspecs))
        return esym_nil;

    git_push_options options;
    egit_push_options_parse(env, opts, &options);
    if (env->non_local_exit_check(env)) {
        egit_strarray_dispose(&refspecs);
        return esym_nil;
    }

    git_remote *remote = EGIT_EXTRACT(_remote);
    int retval = git_remote_push(remote, &refspecs, &options);

    egit_strarray_dispose(&refspecs);
    egit_push_options_release(&options);

    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}
