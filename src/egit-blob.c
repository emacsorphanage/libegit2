#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-blob.h"


// =============================================================================
// Constructors

EGIT_DOC(blob_create_fromstring, "REPO STR",
         "Create a new blob in REPO from the string STR."
         "If STR is multibyte, it will be UTF-8 encoded.");
emacs_value egit_blob_create_fromstring(emacs_env *env, emacs_value _repo, emacs_value _str)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_str);

    git_repository *repo = EGIT_EXTRACT(_repo);
    ptrdiff_t size;
    void *data = em_get_string_with_size(env, _str, &size);

    git_oid oid;
    int retval = git_blob_create_frombuffer(&oid, repo, data, size);
    free(data);
    EGIT_CHECK_ERROR(retval);

    const char *oid_s = git_oid_tostr_s(&oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(blob_create_fromdisk, "REPO PATH",
         "Create a new blob in REPO from the file at PATH and return its ID.");
emacs_value egit_blob_create_fromdisk(emacs_env *env, emacs_value _repo, emacs_value _path)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_path);
    EM_NORMALIZE_PATH(_path);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *path = EM_EXTRACT_STRING(_path);
    git_oid oid;
    int retval = git_blob_create_fromdisk(&oid, repo, path);
    free(path);
    EGIT_CHECK_ERROR(retval);

    const char *oid_s = git_oid_tostr_s(&oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(blob_create_fromworkdir, "REPO PATH",
         "Create a new blob in REPO from the file at workdir PATH and return its ID.");
emacs_value egit_blob_create_fromworkdir(emacs_env *env, emacs_value _repo, emacs_value _path)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_path);

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *path = EM_EXTRACT_STRING(_path);
    git_oid oid;
    int retval = git_blob_create_fromworkdir(&oid, repo, path);
    free(path);
    EGIT_CHECK_ERROR(retval);

    const char *oid_s = git_oid_tostr_s(&oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(blob_lookup, "REPO OID", "Look up a blob in REPO by OID.");
emacs_value egit_blob_lookup(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    EGIT_EXTRACT_OID(_oid, oid);

    git_blob *blob;
    int retval = git_blob_lookup(&blob, repo, &oid);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_BLOB, blob, EM_EXTRACT_USER_PTR(_repo));
}

EGIT_DOC(blob_lookup_prefix, "REPO OID", "Lookup a blob in REPO by shortened OID.");
emacs_value egit_blob_lookup_prefix(emacs_env *env, emacs_value _repo, emacs_value _oid)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_oid);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_oid oid;
    size_t len;
    EGIT_EXTRACT_OID_PREFIX(_oid, oid, len);

    git_blob *blob;
    int retval = git_blob_lookup_prefix(&blob, repo, &oid, len);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_BLOB, blob, EM_EXTRACT_USER_PTR(_repo));
}


// =============================================================================
// Getters

EGIT_DOC(blob_binary_p, "BLOB",
         "Non-nil if BLOB is binary.\n"
         "Note: This check is only heuristic!");
emacs_value egit_blob_binary_p(emacs_env *env, emacs_value _blob)
{
    EGIT_ASSERT_BLOB(_blob);
    git_blob *blob = EGIT_EXTRACT(_blob);
    return git_blob_is_binary(blob) ? esym_t : esym_nil;
}

EGIT_DOC(blob_filtered_content, "BLOB PATH &optional IGNORE-BINARY",
         "Get the filtered content of BLOB as a unibyte string.\n"
         "PATH is used for file attribute lookups.\n"
         "If IGNORE-BINARY is non-nil, no checks are made for whether\n"
         "the blob looks like binary data before applying filters.");
emacs_value egit_blob_filtered_content(
    emacs_env *env, emacs_value _blob, emacs_value _path, emacs_value ignore)
{
    EGIT_ASSERT_BLOB(_blob);
    EM_ASSERT_STRING(_path);

    git_blob *blob = EGIT_EXTRACT(_blob);
    char *path = EM_EXTRACT_STRING(_path);

    git_buf buf = {NULL};
    int retval = git_blob_filtered_content(&buf, blob, path, !EM_EXTRACT_BOOLEAN(ignore));
    free(path);
    EGIT_CHECK_ERROR(retval);

    emacs_value str = env->make_string(env, buf.ptr, buf.size);
    git_buf_dispose(&buf);
    return em_string_as_unibyte(env, str);
}

EGIT_DOC(blob_id, "BLOB", "Return the ID of BLOB.");
emacs_value egit_blob_id(emacs_env *env, emacs_value _blob)
{
    EGIT_ASSERT_BLOB(_blob);
    git_blob *blob = EGIT_EXTRACT(_blob);
    const git_oid *oid = git_blob_id(blob);
    const char *oid_s = git_oid_tostr_s(oid);
    return EM_STRING(oid_s);
}

EGIT_DOC(blob_owner, "BLOB", "Return the repository that BLOB belongs to.");
emacs_value egit_blob_owner(emacs_env *env, emacs_value _blob)
{
    EGIT_ASSERT_BLOB(_blob);
    egit_object *owner = EGIT_EXTRACT_PARENT(_blob);
    owner->refcount++;
    return EM_USER_PTR(owner, egit_finalize);
}

EGIT_DOC(blob_rawcontent, "BLOB", "Get the raw content of BLOB as a unibyte string.");
emacs_value egit_blob_rawcontent(emacs_env *env, emacs_value _blob)
{
    EGIT_ASSERT_BLOB(_blob);
    git_blob *blob = EGIT_EXTRACT(_blob);
    const void *content = git_blob_rawcontent(blob);
    emacs_value str = env->make_string(env, content, git_blob_rawsize(blob));
    return em_string_as_unibyte(env, str);
}

EGIT_DOC(blob_rawsize, "BLOB", "Get the number of bytes in BLOB.");
emacs_value egit_blob_rawsize(emacs_env *env, emacs_value _blob)
{
    EGIT_ASSERT_BLOB(_blob);
    git_blob *blob = EGIT_EXTRACT(_blob);
    git_off_t size = git_blob_rawsize(blob);
    return EM_INTEGER(size);
}
