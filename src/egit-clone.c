#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


EGIT_DOC(clone, "URL PATH", "Clone the repository at URL to PATH and return it.");
emacs_value egit_clone(emacs_env *env, emacs_value _url, emacs_value _path)
{
    EM_ASSERT_STRING(_url);
    EM_ASSERT_STRING(_path);
    EM_NORMALIZE_PATH(_path);

    git_repository *repo;
    int retval;
    {
        char *url = EM_EXTRACT_STRING(_url);
        char *path = EM_EXTRACT_STRING(_path);
        retval = git_clone(&repo, url, path, NULL);
        free(url);
        free(path);
    }
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_REPOSITORY, repo, NULL);
}
