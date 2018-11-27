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
    emacs_value sideband_progress;
    emacs_value certificate_check;
    emacs_value credentials;
    emacs_value transfer_progress;
} remote_ctx;

static int sideband_progress_cb(const char *str, int len, void *payload)
{
    remote_ctx *ctx = (remote_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value msg = env->make_string(env, str, len);
    env->funcall(env, ctx->sideband_progress, 1, &msg);

    EM_RETURN_IF_NLE(GIT_EUSER);
    return 0;
}

static int certificate_check_cb(git_cert *cert, int valid, const char *host, void *payload)
{
    remote_ctx *ctx = (remote_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[3];
    args[0] = em_nil;
    args[1] = valid ? em_t : em_nil;
    args[2] = host ? EM_STRING(host) : em_nil;

    // TODO: GIT_CERT_STRARRAY?
    if (cert->cert_type == GIT_CERT_X509) {
        git_cert_x509 *c = (git_cert_x509*) cert;
        emacs_value data = env->make_string(env, c->data, c->len);
        data = em_string_as_unibyte(env, data);
        args[0] = em_cons(env, em_x509, em_cons(env, data, em_nil));
    }
    else if (cert->cert_type == GIT_CERT_HOSTKEY_LIBSSH2) {
        git_cert_hostkey *c = (git_cert_hostkey*) cert;

        size_t nelts = 0;
        emacs_value elts[5];
        elts[nelts++] = em_hostkey_libssh2;

        // We use libgit's own oid_tostr to decode hashes into hex strings
        char buf[41];
        if (c->type & GIT_CERT_SSH_MD5) {
            elts[nelts++] = em_md5;
            git_oid_tostr(buf, 33, (git_oid*) c->hash_md5);
            elts[nelts++] = EM_STRING(buf);
        }
        if (c->type & GIT_CERT_SSH_SHA1) {
            elts[nelts++] = em_sha1;
            git_oid_tostr(buf, 41, (git_oid*) c->hash_sha1);
            elts[nelts++] = EM_STRING(buf);
        }

        args[0] = em_list(env, elts, nelts);
    }

    env->funcall(env, ctx->certificate_check, 3, args);
    EM_RETURN_IF_NLE(GIT_EUSER);
    return 0;
}

static int credentials_cb(
    git_cred **cred, const char *url, const char *username_from_url,
    unsigned int allowed_types, void *payload)
{
    remote_ctx *ctx = (remote_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[3];
    args[0] = EM_STRING(url);
    args[1] = username_from_url ? EM_STRING(username_from_url) : em_nil;

    emacs_value types[7];
    size_t ntypes = 0;
    if (allowed_types & GIT_CREDTYPE_USERPASS_PLAINTEXT)
        types[ntypes++] = em_userpass_plaintext;
    if (allowed_types & GIT_CREDTYPE_SSH_KEY)
        types[ntypes++] = em_ssh_key;
    if (allowed_types & GIT_CREDTYPE_SSH_CUSTOM)
        types[ntypes++] = em_ssh_custom;
    if (allowed_types & GIT_CREDTYPE_DEFAULT)
        types[ntypes++] = em_default;
    if (allowed_types & GIT_CREDTYPE_SSH_INTERACTIVE)
        types[ntypes++] = em_ssh_interactive;
    if (allowed_types & GIT_CREDTYPE_USERNAME)
        types[ntypes++] = em_username;
    if (allowed_types & GIT_CREDTYPE_SSH_MEMORY)
        types[ntypes++] = em_ssh_memory;
    args[2] = em_list(env, types, ntypes);

    emacs_value retval = env->funcall(env, ctx->credentials, 3, args);
    EM_RETURN_IF_NLE(GIT_EUSER);

    if (egit_get_type(env, retval) != EGIT_CRED) {
        em_signal_wrong_type(env, em_libgit_cred_p, retval);
        return GIT_EUSER;
    }

    // The transport object takes ownership of the credential,
    // so we must duplicate it here.
    return egit_cred_dup(cred, EGIT_EXTRACT(retval));
}

static int transfer_progress_cb(const git_transfer_progress *stats, void *payload)
{
    remote_ctx *ctx = (remote_ctx*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[7];
    args[0] = EM_INTEGER(stats->total_objects);
    args[1] = EM_INTEGER(stats->indexed_objects);
    args[2] = EM_INTEGER(stats->received_objects);
    args[3] = EM_INTEGER(stats->local_objects);
    args[4] = EM_INTEGER(stats->total_deltas);
    args[5] = EM_INTEGER(stats->indexed_deltas);
    args[6] = EM_INTEGER(stats->received_bytes);

    env->funcall(env, ctx->transfer_progress, 7, args);
    EM_RETURN_IF_NLE(GIT_EUSER);
    return 0;
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

#define CHECK(sym, var)                         \
    if (EM_EQ(car, (sym))) {                    \
        EM_ASSERT_FUNCTION(cdr);                \
        (var) = cdr;                            \
        found = true;                           \
    }

#define STORE(sym, var)                         \
    if (EM_EXTRACT_BOOLEAN((var))) {            \
        opts->sym = &sym##_cb;                  \
        payload->sym = (var);                   \
    }

static emacs_value callbacks_parse(emacs_env *env, emacs_value alist, git_remote_callbacks *opts)
{
    emacs_value side = em_nil,
                cred = em_nil,
                cert = em_nil,
                prog = em_nil;
    bool found = false;

    emacs_value car, cdr;
    {
        EM_DOLIST(cell, alist, options);
        EM_ASSERT_CONS(cell);
        car = em_car(env, cell);
        cdr = em_cdr(env, cell);

        CHECK(em_sideband_progress, side);
        CHECK(em_credentials, cred);
        CHECK(em_certificate_check, cert);
        CHECK(em_transfer_progress, prog);

        EM_DOLIST_END(options);
    }

    if (found) {
        remote_ctx *payload = (remote_ctx*) malloc(sizeof(remote_ctx));
        *payload = (remote_ctx) {.env = env};
        opts->payload = payload;
        STORE(sideband_progress, side);
        STORE(credentials, cred);
        STORE(certificate_check, cert);
        STORE(transfer_progress, prog);
    }

    return em_nil;
}

#undef CHECK
#undef STORE

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
