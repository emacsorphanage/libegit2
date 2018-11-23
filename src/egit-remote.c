#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
#include "interface.h"
#include "egit-remote.h"


// =============================================================================
// Callbacks

typedef struct {
    emacs_env *env;
    emacs_value certificate_check;
    emacs_value credentials;
} remote_ctx;

int certificate_check_cb(
    __attribute__((unused)) git_cert *cert,
    __attribute__((unused)) int valid,
    __attribute__((unused)) const char *host,
    __attribute__((unused)) void *payload)
{
    return GIT_EUSER;
}

int credentials_cb(
    __attribute__((unused)) git_cred **cred,
    __attribute__((unused)) const char *url,
    __attribute__((unused)) const char *username_from_url,
    __attribute__((unused)) unsigned int allowed_types,
    __attribute__((unused)) void *payload)
{
    return GIT_EUSER;
}


// =============================================================================
// Helpers - option parsing

static emacs_value proxy_parse(emacs_env *env, emacs_value alist, git_proxy_options *opts)
{
    emacs_value url = em_nil;
    emacs_value cred = em_nil;
    emacs_value cert = em_nil;

    emacs_value car, cdr;
    {
        EM_DOLIST(cell, alist, options);
        EM_ASSERT_CONS(cell);
        car = em_car(env, cell);
        cdr = em_cdr(env, cell);

        if (EM_EQ(car, em_type)) {
            if (!EM_EXTRACT_BOOLEAN(cdr))
                opts->type = GIT_PROXY_NONE;
            else if (EM_EQ(cdr, em_auto))
                opts->type = GIT_PROXY_AUTO;
            else if (EM_EQ(cdr, em_specified))
                opts->type = GIT_PROXY_SPECIFIED;
            else {
                em_signal_wrong_value(env, cdr);
                return em_nil;
            }
        }
        else if (EM_EQ(car, em_url)) {
            EM_ASSERT_STRING(cdr);
            url = cdr;
        }
        else if (EM_EQ(car, em_credentials)) {
            EM_ASSERT_FUNCTION(cdr);
            cred = cdr;
        }
        else if (EM_EQ(car, em_certificate_check)) {
            EM_ASSERT_FUNCTION(cdr);
            cert = cdr;
        }

        EM_DOLIST_END(options);
    }

    if (EM_EXTRACT_BOOLEAN(url))
        opts->url = EM_EXTRACT_STRING(url);
    if (EM_EXTRACT_BOOLEAN(cred) || EM_EXTRACT_BOOLEAN(cert)) {
        remote_ctx *payload = (remote_ctx*) malloc(sizeof(remote_ctx));
        *payload = (remote_ctx) {.env = env, .credentials = cred, .certificate_check = cert};
        if (EM_EXTRACT_BOOLEAN(cred))
            opts->credentials = &credentials_cb;
        if (EM_EXTRACT_BOOLEAN(cert))
            opts->certificate_check = &certificate_check_cb;
    }

    return em_nil;
}

static emacs_value callbacks_parse(emacs_env *env, emacs_value alist, git_remote_callbacks *opts)
{
    emacs_value cred = em_nil;
    emacs_value cert = em_nil;
    bool found = false;

    emacs_value car, cdr;
    {
        EM_DOLIST(cell, alist, options);
        EM_ASSERT_CONS(cell);
        car = em_car(env, cell);
        cdr = em_cdr(env, cell);

        if (EM_EQ(car, em_credentials)) {
            EM_ASSERT_FUNCTION(cdr);
            cred = cdr;
            found = true;
        }
        else if (EM_EQ(car, em_certificate_check)) {
            EM_ASSERT_FUNCTION(cdr);
            cert = cdr;
            found = true;
        }

        EM_DOLIST_END(options);
    }

    if (found) {
        remote_ctx *payload = (remote_ctx*) malloc(sizeof(remote_ctx));
        *payload = (remote_ctx) {
            .env = env,
            .credentials = cred,
            .certificate_check = cert
        };
        opts->payload = payload;
        if (EM_EXTRACT_BOOLEAN(cred))
            opts->credentials = &credentials_cb;
        if (EM_EXTRACT_BOOLEAN(cert))
            opts->certificate_check = &certificate_check_cb;
    }

    return em_nil;
}

static void fetch_options_dispose(git_fetch_options *opts)
{
    egit_strarray_dispose(&opts->custom_headers);
    free(opts->proxy_opts.payload);
    free((void*) opts->proxy_opts.url);
    free(opts->callbacks.payload);
}

static emacs_value fetch_options_parse(emacs_env *env, emacs_value alist, git_fetch_options *opts)
{
    int retval = git_fetch_init_options(opts, GIT_FETCH_OPTIONS_VERSION);
    EGIT_CHECK_ERROR(retval);

    emacs_value callbacks = em_nil;
    emacs_value headers = em_nil;
    emacs_value proxy = em_nil;

    // Main loop through the options alist
    emacs_value car, cdr;
    {
        EM_DOLIST(cell, alist, options);
        car = em_car(env, cell);
        cdr = em_cdr(env, cell);

        if (EM_EQ(car, em_callbacks))
            callbacks = cdr;
        else if (EM_EQ(car, em_headers))
            headers = cdr;
        else if (EM_EQ(car, em_proxy))
            proxy = cdr;
        else if (EM_EQ(car, em_prune)) {
            if (!EM_EXTRACT_BOOLEAN(cdr))
                opts->prune = GIT_FETCH_PRUNE_UNSPECIFIED;
            else if (EM_EQ(cdr, em_on))
                opts->prune = GIT_FETCH_PRUNE;
            else if (EM_EQ(cdr, em_off))
                opts->prune = GIT_FETCH_NO_PRUNE;
            else {
                em_signal_wrong_value(env, cdr);
                return em_nil;
            }
        }
        else if (EM_EQ(car, em_download_tags)) {
            if (!EM_EXTRACT_BOOLEAN(cdr))
                opts->download_tags = GIT_REMOTE_DOWNLOAD_TAGS_UNSPECIFIED;
            else if (EM_EQ(cdr, em_auto))
                opts->download_tags = GIT_REMOTE_DOWNLOAD_TAGS_AUTO;
            else if (EM_EQ(cdr, em_none))
                opts->download_tags = GIT_REMOTE_DOWNLOAD_TAGS_NONE;
            else if (EM_EQ(cdr, em_all))
                opts->download_tags = GIT_REMOTE_DOWNLOAD_TAGS_ALL;
            else {
                em_signal_wrong_value(env, cdr);
                return em_nil;
            }
        }
        else if (EM_EQ(car, em_update_fetchhead))
            opts->update_fetchhead = EM_EXTRACT_BOOLEAN(cdr);

        EM_DOLIST_END(options);
    }

    if (EM_EXTRACT_BOOLEAN(callbacks)) {
        callbacks_parse(env, callbacks, &opts->callbacks);
        if (env->non_local_exit_check(env))
            goto cleanup;
    }
    if (EM_EXTRACT_BOOLEAN(headers)) {
        if (!egit_strarray_from_list(&opts->custom_headers, env, headers))
            goto cleanup;
    }
    if (EM_EXTRACT_BOOLEAN(proxy)) {
        proxy_parse(env, proxy, &opts->proxy_opts);
        if (env->non_local_exit_check(env))
            goto cleanup;
    }

    return em_nil;

  cleanup:
    fetch_options_dispose(opts);
    return em_nil;
}

static void push_options_dispose(git_push_options *opts)
{
    egit_strarray_dispose(&opts->custom_headers);
    free(opts->proxy_opts.payload);
    free((void*) opts->proxy_opts.url);
    free(opts->callbacks.payload);
}

static emacs_value push_options_parse(emacs_env *env, emacs_value alist, git_push_options *opts)
{
    int retval = git_push_init_options(opts, GIT_PUSH_OPTIONS_VERSION);
    EGIT_CHECK_ERROR(retval);

    emacs_value callbacks = em_nil;
    emacs_value headers = em_nil;
    emacs_value proxy = em_nil;

    // Main loop through the options alist
    emacs_value car, cdr;
    {
        EM_DOLIST(cell, alist, options);
        EM_ASSERT_CONS(cell);
        car = em_car(env, cell);
        cdr = em_cdr(env, cell);

        if (EM_EQ(car, em_callbacks))
            callbacks = cdr;
        else if (EM_EQ(car, em_headers))
            headers = cdr;
        else if (EM_EQ(car, em_proxy))
            proxy = cdr;
        else if (EM_EQ(car, em_threads)) {
            if (!EM_EXTRACT_BOOLEAN(cdr))
                opts->pb_parallelism = 0;
            EM_ASSERT_INTEGER(cdr);
            opts->pb_parallelism = EM_EXTRACT_INTEGER(cdr);
        }

        EM_DOLIST_END(options);
    }

    if (EM_EXTRACT_BOOLEAN(callbacks)) {
        callbacks_parse(env, callbacks, &opts->callbacks);
        if (env->non_local_exit_check(env))
            goto cleanup;
    }
    if (EM_EXTRACT_BOOLEAN(headers)) {
        if (!egit_strarray_from_list(&opts->custom_headers, env, headers))
            goto cleanup;
    }
    if (EM_EXTRACT_BOOLEAN(proxy)) {
        proxy_parse(env, proxy, &opts->proxy_opts);
        if (env->non_local_exit_check(env))
            goto cleanup;
    }

    return em_nil;

  cleanup:
    push_options_dispose(opts);
    return em_nil;
}


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

    return egit_wrap(env, EGIT_REMOTE, remote, NULL);
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

    return egit_wrap(env, EGIT_REMOTE, remote, NULL);
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
    switch (ret) {
    case GIT_REMOTE_DOWNLOAD_TAGS_AUTO: return em_auto;
    case GIT_REMOTE_DOWNLOAD_TAGS_NONE: return em_none;
    case GIT_REMOTE_DOWNLOAD_TAGS_ALL: return em_all;
    default: return em_nil;
    }
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
        return em_nil;
    }

    return egit_wrap(env, EGIT_REFSPEC, refspec, EM_EXTRACT_USER_PTR(_remote));
}

EGIT_DOC(remote_get_refspecs, "REMOTE DIRECTION",
         "Get the list of the refspecs of REMOTE.\n"
         "DIRECTION is either `push' or `fetch'.");
emacs_value egit_remote_get_refspecs(emacs_env *env, emacs_value _remote, emacs_value dir)
{
    EGIT_ASSERT_REMOTE(_remote);
    git_remote *remote = EGIT_EXTRACT(_remote);

    git_strarray out = {NULL, 0};
    int retval;
    if (EM_EQ(dir, em_fetch))
        retval = git_remote_get_fetch_refspecs(&out, remote);
    else if (EM_EQ(dir, em_push))
        retval = git_remote_get_push_refspecs(&out, remote);
    else {
        em_signal_wrong_value(env, dir);
        return em_nil;
    }
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
    return em_nil;
}

EGIT_DOC(remote_owner, "REMOTE", "Get the repository that REMOTE belongs to.");
emacs_value egit_remote_owner(emacs_env *env, emacs_value _remote)
{
    EGIT_ASSERT_REMOTE(_remote);
    git_remote *remote = EGIT_EXTRACT(_remote);
    git_repository *repo = git_remote_owner(remote);
    return egit_wrap(env, EGIT_REPOSITORY, repo, NULL);
}

EGIT_DOC(remote_pushurl, "REMOTE", "Get the push URL of REMOTE, or nil if none set.");
emacs_value egit_remote_pushurl(emacs_env *env, emacs_value _remote)
{
    EGIT_ASSERT_REMOTE(_remote);
    git_remote *remote = EGIT_EXTRACT(_remote);
    const char *url = git_remote_pushurl(remote);
    if (url)
        return EM_STRING(url);
    return em_nil;
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
    return retval ? em_t : em_nil;
}


// =============================================================================
// Operations

EGIT_DOC(remote_add_refspec, "REPO NAME REFSPEC DIRECTION",
         "Add a new REFSPEC to the remote named NAME in REPO.\n"
         "DIRECTION may be either `fetch' or `push'.");
emacs_value egit_remote_add_refspec(
    emacs_env *env, emacs_value _repo, emacs_value _name,
    emacs_value _refspec, emacs_value direction)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_name);
    EM_ASSERT_STRING(_refspec);

    bool push;
    if (EM_EQ(direction, em_push))
        push = true;
    else if (EM_EQ(direction, em_fetch))
        push = false;
    else {
        em_signal_wrong_value(env, direction);
        return em_nil;
    }

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *name = EM_EXTRACT_STRING(_name);
    char *refspec = EM_EXTRACT_STRING(_refspec);

    int retval = push ? git_remote_add_push(repo, name, refspec) :
                 git_remote_add_fetch(repo, name, refspec);
    free(name);
    free(refspec);
    EGIT_CHECK_ERROR(retval);

    return em_nil;
}

EGIT_DOC(remote_fetch, "REMOTE &optional REFSPECS OPTIONS MESSAGE", "");
emacs_value egit_remote_fetch(
    emacs_env *env, emacs_value _remote, emacs_value _refspecs, emacs_value opts, emacs_value _msg)
{
    EGIT_ASSERT_REMOTE(_remote);
    EM_ASSERT_STRING_OR_NIL(_msg);

    git_strarray refspecs;
    if (!egit_strarray_from_list(&refspecs, env, _refspecs))
        return em_nil;

    git_fetch_options options;
    fetch_options_parse(env, opts, &options);
    if (env->non_local_exit_check(env)) {
        egit_strarray_dispose(&refspecs);
        return em_nil;
    }

    git_remote *remote = EGIT_EXTRACT(_remote);
    char *msg = EM_EXTRACT_STRING_OR_NULL(_msg);
    int retval = git_remote_fetch(remote, &refspecs, &options, msg);

    free(msg);
    egit_strarray_dispose(&refspecs);
    fetch_options_dispose(&options);

    EGIT_CHECK_ERROR(retval);
    return em_nil;
}

EGIT_DOC(remote_push, "REMOTE &optional REFSPECS OPTIONS", "");
emacs_value egit_remote_push(emacs_env *env, emacs_value _remote, emacs_value _refspecs, emacs_value opts)
{
    EGIT_ASSERT_REMOTE(_remote);

    git_strarray refspecs;
    if (!egit_strarray_from_list(&refspecs, env, _refspecs))
        return em_nil;

    git_push_options options;
    push_options_parse(env, opts, &options);
    if (env->non_local_exit_check(env)) {
        egit_strarray_dispose(&refspecs);
        return em_nil;
    }

    git_remote *remote = EGIT_EXTRACT(_remote);
    int retval = git_remote_push(remote, &refspecs, &options);

    egit_strarray_dispose(&refspecs);
    push_options_dispose(&options);

    EGIT_CHECK_ERROR(retval);
    return em_nil;
}
