#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
#include "interface.h"
#include "egit-options.h"


/*
 * This file contains functions for parsing Emacs alists into various
 * git_XYZ_options structs. There are others scattered around in src/egit-XYZ.c,
 * but some options are needed in more than one location. We should move all
 * of them here eventually.
 *
 * A parse function should have the signature
 *   emacs_value egit_XYZ_options_parse(emacs_env *, emacs_value, git_XYZ_options*)
 *
 * The return value is useless, and is there just to facilitate using the
 * various ASSERT macros, which return esym_nil on failure. Every parse function
 * should make sure to signal an Emacs error in case of problems.
 *
 * There should also be an egit_XYZ_options_release for freeing data that may
 * have been allocated by the parse function, if applicable.
 */


// =============================================================================
// Checkout

static int checkout_notify_callback(
    git_checkout_notify_t why,
    const char *path,
    __attribute__((unused)) const git_diff_file *baseline,
    __attribute__((unused)) const git_diff_file *target,
    __attribute__((unused)) const git_diff_file *workdir,
    void *payload)
{
    egit_generic_payload *ctx = (egit_generic_payload*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[2];
    args[0] = esym_nil;
    args[1] = EM_STRING(path);

    switch (why) {
    case GIT_CHECKOUT_NOTIFY_IGNORED:
        args[0] = esym_ignored;
        break;
    case GIT_CHECKOUT_NOTIFY_UNTRACKED:
        args[0] = esym_untracked;
        break;
    case GIT_CHECKOUT_NOTIFY_UPDATED:
        args[0] = esym_updated;
        break;
    case GIT_CHECKOUT_NOTIFY_DIRTY:
        args[0] = esym_dirty;
        break;
    case GIT_CHECKOUT_NOTIFY_CONFLICT:
        args[0] = esym_conflict;
        break;
    default: break;
    }

    emacs_value retval = env->funcall(env, ctx->func, 2, args);
    EM_RETURN_IF_NLE(GIT_EUSER);

    if (EM_EQ(retval, esym_abort))
        return GIT_EUSER;
    return 0;
}

static void checkout_progress_callback(
    const char *path,
    size_t completed_steps,
    size_t total_steps,
    void *payload)
{
    egit_generic_payload *ctx = (egit_generic_payload*) payload;
    emacs_env *env = ctx->env;

    emacs_value args[3];
    args[0] = path ? EM_STRING(path) : esym_nil;
    args[1] = EM_INTEGER(completed_steps);
    args[2] = EM_INTEGER(total_steps);

    env->funcall(env, ctx->func, 3, args);

    // Since we can't abort from inside a progress callback, we clear
    // non-local exits, essentially running the whole callback
    // in an implicit (ignore-errors ...) form.
    if (env->non_local_exit_check(env))
        env->non_local_exit_clear(env);
}

emacs_value egit_checkout_options_parse(emacs_env *env, emacs_value alist, git_checkout_options *opts)
{
    git_checkout_init_options(opts, GIT_CHECKOUT_OPTIONS_VERSION);

    emacs_value notify_callback = esym_nil;
    emacs_value progress_callback = esym_nil;

    // Main loop through the options alist
    {
        emacs_value car, cdr;
        EM_DOLIST(option, alist, options);
        EM_ASSERT_CONS(option);

        car = em_car(env, option);
        cdr = em_cdr(env, option);

        // TODO: Support the whole range of checkout strategies and options
        if (EM_EQ(car, esym_strategy)) {
            if (!em_findsym_checkout_strategy(&opts->checkout_strategy, env, cdr, true))
                return esym_nil;
        }
        else if (EM_EQ(car, esym_notify_when)) {
            if (EM_EQ(cdr, esym_all))
                opts->notify_flags |= GIT_CHECKOUT_NOTIFY_ALL;
            else {
                EM_DOLIST(icar, cdr, notify_when);
                if (EM_EQ(icar, esym_conflict))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_CONFLICT;
                else if (EM_EQ(icar, esym_dirty))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_DIRTY;
                else if (EM_EQ(icar, esym_updated))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_UPDATED;
                else if (EM_EQ(icar, esym_untracked))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_UNTRACKED;
                else if (EM_EQ(icar, esym_ignored))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_IGNORED;
                else if (EM_EQ(icar, esym_all))
                    opts->notify_flags |= GIT_CHECKOUT_NOTIFY_ALL;
                EM_DOLIST_END(notify_when);
            }
        }
        else if (EM_EQ(car, esym_notify)) {
            EM_ASSERT_FUNCTION(cdr);
            notify_callback = cdr;
        }
        else if (EM_EQ(car, esym_progress)) {
            EM_ASSERT_FUNCTION(cdr);
            progress_callback = cdr;
        }
        else if (EM_EQ(car, esym_baseline)) {
            egit_type type = egit_get_type(env, cdr);
            if (type == EGIT_TREE)
                opts->baseline = EGIT_EXTRACT(cdr);
            else if (type == EGIT_INDEX)
                opts->baseline_index = EGIT_EXTRACT(cdr);
            else {
                // TODO: libgit-tree-or-index-p?
                em_signal_wrong_type(env, esym_libgit_tree_p, cdr);
                return esym_nil;
            }
        }

        EM_DOLIST_END(options);
    }

    if (EM_EXTRACT_BOOLEAN(notify_callback)) {
        egit_generic_payload *ctx = (egit_generic_payload*) malloc(sizeof(egit_generic_payload));
        ctx->env = env;
        ctx->func = notify_callback;
        opts->notify_payload = ctx;
        opts->notify_cb = &checkout_notify_callback;
    }

    if (EM_EXTRACT_BOOLEAN(progress_callback)) {
        egit_generic_payload *ctx = (egit_generic_payload*) malloc(sizeof(egit_generic_payload));
        ctx->env = env;
        ctx->func = progress_callback;
        opts->progress_payload = ctx;
        opts->progress_cb = &checkout_progress_callback;
    }

    return esym_nil;
}

void egit_checkout_options_release(git_checkout_options *opts)
{
    free(opts->notify_payload);
    free(opts->progress_payload);
}


// =============================================================================
// Merge

emacs_value egit_merge_options_parse(emacs_env *env, emacs_value alist, git_merge_options *opts)
{
    git_merge_init_options(opts, GIT_MERGE_OPTIONS_VERSION);

    emacs_value file_flags = esym_nil;

    // Main loop through the options alist
    {
        emacs_value car, cdr;
        EM_DOLIST(option, alist, options);
        EM_ASSERT_CONS(option);

        car = em_car(env, option);
        cdr = em_cdr(env, option);

        if (EM_EQ(car, esym_find_renames))
            EGIT_SET_BIT(opts->flags, GIT_MERGE_FIND_RENAMES, cdr);
        else if (EM_EQ(car, esym_fail_on_conflict))
            EGIT_SET_BIT(opts->flags, GIT_MERGE_FAIL_ON_CONFLICT, cdr);
        else if (EM_EQ(car, esym_skip_reuc))
            EGIT_SET_BIT(opts->flags, GIT_MERGE_SKIP_REUC, cdr);
        else if (EM_EQ(car, esym_no_recursive))
            EGIT_SET_BIT(opts->flags, GIT_MERGE_NO_RECURSIVE, cdr);
        else if (EM_EQ(car, esym_rename_threshold))
            opts->rename_threshold = EM_EXTRACT_INTEGER(cdr);
        else if (EM_EQ(car, esym_target_limit))
            opts->target_limit = EM_EXTRACT_INTEGER(cdr);
        else if (EM_EQ(car, esym_recursion_limit))
            opts->recursion_limit = EM_EXTRACT_INTEGER(cdr);
        else if (EM_EQ(car, esym_default_driver)) {
            EM_ASSERT_STRING(cdr);
            opts->default_driver = EM_EXTRACT_STRING(cdr);
        }
        else if (EM_EQ(car, esym_file_favor)) {
            if (!em_findsym_merge_file_favor(&opts->file_favor, env, cdr, true))
                return esym_nil;
        }
        else if (EM_EQ(car, esym_file_flags))
            file_flags = cdr;

        EM_DOLIST_END(options);
    }

    // File flags
    {
        emacs_value car, cdr;
        EM_DOLIST(option, file_flags, flags);
        EM_ASSERT_CONS(option);

        car = em_car(env, option);
        cdr = em_cdr(env, option);

        if (EM_EQ(car, esym_style_merge))
            EGIT_SET_BIT(opts->file_flags, GIT_MERGE_FILE_STYLE_MERGE, cdr);
        else if (EM_EQ(car, esym_style_diff3))
            EGIT_SET_BIT(opts->file_flags, GIT_MERGE_FILE_STYLE_DIFF3, cdr);
        else if (EM_EQ(car, esym_simplify_alnum))
            EGIT_SET_BIT(opts->file_flags, GIT_MERGE_FILE_SIMPLIFY_ALNUM, cdr);
        else if (EM_EQ(car, esym_ignore_whitespace))
            EGIT_SET_BIT(opts->file_flags, GIT_MERGE_FILE_IGNORE_WHITESPACE, cdr);
        else if (EM_EQ(car, esym_ignore_whitespace_change))
            EGIT_SET_BIT(opts->file_flags, GIT_MERGE_FILE_IGNORE_WHITESPACE_CHANGE, cdr);
        else if (EM_EQ(car, esym_ignore_whitespace_eol))
            EGIT_SET_BIT(opts->file_flags, GIT_MERGE_FILE_IGNORE_WHITESPACE_EOL, cdr);
        else if (EM_EQ(car, esym_patience))
            EGIT_SET_BIT(opts->file_flags, GIT_MERGE_FILE_DIFF_PATIENCE, cdr);
        else if (EM_EQ(car, esym_minimal))
            EGIT_SET_BIT(opts->file_flags, GIT_MERGE_FILE_DIFF_MINIMAL, cdr);
        else {
            em_signal_wrong_value(env, cdr);
            return esym_nil;
        }

        EM_DOLIST_END(flags);
    }

    return esym_nil;
}


// =============================================================================
// Remote callbacks

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
    args[0] = esym_nil;
    args[1] = valid ? esym_t : esym_nil;
    args[2] = host ? EM_STRING(host) : esym_nil;

    // TODO: GIT_CERT_STRARRAY?
    if (cert->cert_type == GIT_CERT_X509) {
        git_cert_x509 *c = (git_cert_x509*) cert;
        emacs_value data = env->make_string(env, c->data, c->len);
        data = em_string_as_unibyte(env, data);
        args[0] = em_cons(env, esym_x509, em_cons(env, data, esym_nil));
    }
    else if (cert->cert_type == GIT_CERT_HOSTKEY_LIBSSH2) {
        git_cert_hostkey *c = (git_cert_hostkey*) cert;

        size_t nelts = 0;
        emacs_value elts[5];
        elts[nelts++] = esym_hostkey_libssh2;

        // We use libgit's own oid_tostr to decode hashes into hex strings
        char buf[41];
        if (c->type & GIT_CERT_SSH_MD5) {
            elts[nelts++] = esym_md5;
            git_oid_tostr(buf, 33, (git_oid*) c->hash_md5);
            elts[nelts++] = EM_STRING(buf);
        }
        if (c->type & GIT_CERT_SSH_SHA1) {
            elts[nelts++] = esym_sha1;
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
    args[1] = username_from_url ? EM_STRING(username_from_url) : esym_nil;

    emacs_value types[7];
    size_t ntypes = 0;
    if (allowed_types & GIT_CREDTYPE_USERPASS_PLAINTEXT)
        types[ntypes++] = esym_userpass_plaintext;
    if (allowed_types & GIT_CREDTYPE_SSH_KEY)
        types[ntypes++] = esym_ssh_key;
    if (allowed_types & GIT_CREDTYPE_SSH_CUSTOM)
        types[ntypes++] = esym_ssh_custom;
    if (allowed_types & GIT_CREDTYPE_DEFAULT)
        types[ntypes++] = esym_default;
    if (allowed_types & GIT_CREDTYPE_SSH_INTERACTIVE)
        types[ntypes++] = esym_ssh_interactive;
    if (allowed_types & GIT_CREDTYPE_USERNAME)
        types[ntypes++] = esym_username;
    if (allowed_types & GIT_CREDTYPE_SSH_MEMORY)
        types[ntypes++] = esym_ssh_memory;
    args[2] = em_list(env, types, ntypes);

    emacs_value retval = env->funcall(env, ctx->credentials, 3, args);
    EM_RETURN_IF_NLE(GIT_EUSER);

    if (egit_get_type(env, retval) != EGIT_CRED) {
        em_signal_wrong_type(env, esym_libgit_cred_p, retval);
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

static emacs_value egit_remote_callbacks_parse(emacs_env *env, emacs_value alist, git_remote_callbacks *opts)
{
    emacs_value side = esym_nil,
                cred = esym_nil,
                cert = esym_nil,
                prog = esym_nil;
    bool found = false;

    emacs_value car, cdr;
    {
        EM_DOLIST(cell, alist, options);
        EM_ASSERT_CONS(cell);
        car = em_car(env, cell);
        cdr = em_cdr(env, cell);

        CHECK(esym_sideband_progress, side);
        CHECK(esym_credentials, cred);
        CHECK(esym_certificate_check, cert);
        CHECK(esym_transfer_progress, prog);

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

    return esym_nil;
}

#undef CHECK
#undef STORE


// =============================================================================
// Proxy

static emacs_value egit_proxy_options_parse(emacs_env *env, emacs_value alist, git_proxy_options *opts)
{
    emacs_value url = esym_nil;
    emacs_value cred = esym_nil;
    emacs_value cert = esym_nil;

    emacs_value car, cdr;
    {
        EM_DOLIST(cell, alist, options);
        EM_ASSERT_CONS(cell);
        car = em_car(env, cell);
        cdr = em_cdr(env, cell);

        if (EM_EQ(car, esym_type)) {
            if (!em_findsym_proxy(&opts->type, env, cdr, true))
                return esym_nil;
        }
        else if (EM_EQ(car, esym_url)) {
            EM_ASSERT_STRING(cdr);
            url = cdr;
        }
        else if (EM_EQ(car, esym_credentials)) {
            EM_ASSERT_FUNCTION(cdr);
            cred = cdr;
        }
        else if (EM_EQ(car, esym_certificate_check)) {
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

    return esym_nil;
}


// =============================================================================
// Fetch

emacs_value egit_fetch_options_parse(emacs_env *env, emacs_value alist, git_fetch_options *opts)
{
    int retval = git_fetch_init_options(opts, GIT_FETCH_OPTIONS_VERSION);
    EGIT_CHECK_ERROR(retval);

    emacs_value callbacks = esym_nil;
    emacs_value headers = esym_nil;
    emacs_value proxy = esym_nil;

    // Main loop through the options alist
    emacs_value car, cdr;
    {
        EM_DOLIST(cell, alist, options);
        car = em_car(env, cell);
        cdr = em_cdr(env, cell);

        if (EM_EQ(car, esym_callbacks))
            callbacks = cdr;
        else if (EM_EQ(car, esym_headers))
            headers = cdr;
        else if (EM_EQ(car, esym_proxy))
            proxy = cdr;
        else if (EM_EQ(car, esym_prune)) {
            if (!em_findsym_fetch_prune(&opts->prune, env, cdr, true))
                return esym_nil;
        }
        else if (EM_EQ(car, esym_download_tags)) {
            if (!em_findsym_remote_autotag_option(&opts->download_tags, env, cdr, true))
                return esym_nil;
        }
        else if (EM_EQ(car, esym_update_fetchhead))
            opts->update_fetchhead = EM_EXTRACT_BOOLEAN(cdr);

        EM_DOLIST_END(options);
    }

    if (EM_EXTRACT_BOOLEAN(callbacks)) {
        egit_remote_callbacks_parse(env, callbacks, &opts->callbacks);
        if (env->non_local_exit_check(env))
            goto cleanup;
    }
    if (EM_EXTRACT_BOOLEAN(headers)) {
        if (!egit_strarray_from_list(&opts->custom_headers, env, headers))
            goto cleanup;
    }
    if (EM_EXTRACT_BOOLEAN(proxy)) {
        egit_proxy_options_parse(env, proxy, &opts->proxy_opts);
        if (env->non_local_exit_check(env))
            goto cleanup;
    }

    return esym_nil;

  cleanup:
    egit_fetch_options_release(opts);
    return esym_nil;
}

void egit_fetch_options_release(git_fetch_options *opts)
{
    egit_strarray_dispose(&opts->custom_headers);
    free(opts->proxy_opts.payload);
    free((void*) opts->proxy_opts.url);
    free(opts->callbacks.payload);
}


// =============================================================================
// Push

emacs_value egit_push_options_parse(emacs_env *env, emacs_value alist, git_push_options *opts)
{
    int retval = git_push_init_options(opts, GIT_PUSH_OPTIONS_VERSION);
    EGIT_CHECK_ERROR(retval);

    emacs_value callbacks = esym_nil;
    emacs_value headers = esym_nil;
    emacs_value proxy = esym_nil;

    // Main loop through the options alist
    emacs_value car, cdr;
    {
        EM_DOLIST(cell, alist, options);
        EM_ASSERT_CONS(cell);
        car = em_car(env, cell);
        cdr = em_cdr(env, cell);

        if (EM_EQ(car, esym_callbacks))
            callbacks = cdr;
        else if (EM_EQ(car, esym_headers))
            headers = cdr;
        else if (EM_EQ(car, esym_proxy))
            proxy = cdr;
        else if (EM_EQ(car, esym_threads)) {
            if (!EM_EXTRACT_BOOLEAN(cdr))
                opts->pb_parallelism = 0;
            EM_ASSERT_INTEGER(cdr);
            opts->pb_parallelism = EM_EXTRACT_INTEGER(cdr);
        }

        EM_DOLIST_END(options);
    }

    if (EM_EXTRACT_BOOLEAN(callbacks)) {
        egit_remote_callbacks_parse(env, callbacks, &opts->callbacks);
        if (env->non_local_exit_check(env))
            goto cleanup;
    }
    if (EM_EXTRACT_BOOLEAN(headers)) {
        if (!egit_strarray_from_list(&opts->custom_headers, env, headers))
            goto cleanup;
    }
    if (EM_EXTRACT_BOOLEAN(proxy)) {
        egit_proxy_options_parse(env, proxy, &opts->proxy_opts);
        if (env->non_local_exit_check(env))
            goto cleanup;
    }

    return esym_nil;

  cleanup:
    egit_push_options_release(opts);
    return esym_nil;
}

void egit_push_options_release(git_push_options *opts)
{
    egit_strarray_dispose(&opts->custom_headers);
    free(opts->proxy_opts.payload);
    free((void*) opts->proxy_opts.url);
    free(opts->callbacks.payload);
}
