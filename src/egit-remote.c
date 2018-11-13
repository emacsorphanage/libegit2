#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-remote.h"


// =============================================================================
// Constructors

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
