#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"


EGIT_DOC(blame_file, "REPOSITORY PATH &optional OPTIONS",
         "Return the BLAME object for the given file PATH.");
emacs_value egit_blame_file(emacs_env *env,
                            emacs_value _repo,
                            emacs_value _path,
                            emacs_value _options)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_path);
    (void)_options; // TODO: handle options as well

    git_repository *repo = EGIT_EXTRACT(_repo);
    char *path = EGIT_EXTRACT_STRING(_path);

    git_blame *blame = NULL;
    int retval = git_blame_file(&blame, repo, path, /*options=*/NULL);
    free(path);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_BLAME, blame);
}


