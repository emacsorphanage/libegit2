#include <string.h>

#include "git2.h"
#include "interface.h"

bool egit_strarray_from_list(git_strarray *array, emacs_env *env, emacs_value list)
{
    array->count = 0;
    array->strings = NULL;

    ptrdiff_t nelems = em_assert_list(env, esym_stringp, list);
    if (nelems < 0)
        return false;
    if (nelems == 0)
        return true;

    array->count = nelems;
    array->strings = (char**) malloc(nelems * sizeof(char*));
    for (ptrdiff_t i = 0; i < nelems; i++) {
        emacs_value car = em_car(env, list);
        array->strings[i] = EM_EXTRACT_STRING(car);
        list = em_cdr(env, list);
    }

    return true;
}

void egit_strarray_dispose(git_strarray *array)
{
    if (array->strings) {
        for (size_t i = 0; i < array->count; i++)
            free(array->strings[i]);
        free(array->strings);
    }
}

int egit_cred_dup(git_cred **out, git_cred *cred)
{
    switch (cred->credtype) {
    case GIT_CREDTYPE_SSH_KEY: {
        git_cred_ssh_key *c = (git_cred_ssh_key*) cred;
        if (c->privatekey)
            return git_cred_ssh_key_new(out, c->username, c->publickey, c->privatekey, c->passphrase);
        return git_cred_ssh_key_from_agent(out, c->username);
    }
    case GIT_CREDTYPE_SSH_MEMORY: {
        git_cred_ssh_key *c = (git_cred_ssh_key*) cred;
        return git_cred_ssh_key_memory_new(out, c->username, c->publickey, c->privatekey, c->passphrase);
    }
    case GIT_CREDTYPE_USERPASS_PLAINTEXT: {
        git_cred_userpass_plaintext *c = (git_cred_userpass_plaintext*) cred;
        return git_cred_userpass_plaintext_new(out, c->username, c->password);
    }
    case GIT_CREDTYPE_USERNAME: {
        git_cred_username *c = (git_cred_username*) cred;
        return git_cred_username_new(out, c->username);
    }
    case GIT_CREDTYPE_DEFAULT:
        return git_cred_default_new(out);
    case GIT_CREDTYPE_SSH_CUSTOM:
    case GIT_CREDTYPE_SSH_INTERACTIVE:
    default:
        giterr_set_str(GITERR_INVALID, "Unsupported credential type");
        return -1;
    }
}

// TODO: This should probably be exposed as an opaque structure too,
// for consistency.
emacs_value egit_tree_entry_to_emacs(emacs_env *env, const git_tree_entry *entry)
{
    git_filemode_t mode = git_tree_entry_filemode(entry);
    git_otype type = git_tree_entry_type(entry);
    const git_oid *oid = git_tree_entry_id(entry);
    const char *oid_s = git_oid_tostr_s(oid);
    const char *name = git_tree_entry_name(entry);

    emacs_value list_args[4];

    list_args[0] = em_findenum_filemode(mode);
    list_args[1] = em_findenum_otype(type);
    list_args[2] = EM_STRING(oid_s);
    list_args[3] = EM_STRING(name);

    return em_list(env, list_args, 4);
}
