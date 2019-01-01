#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-pathspec.h"

EGIT_DOC(pathspec_new, "PATHSPECS",
         "Compile a pathspec object from a PATHSPECS list of strings.");
emacs_value egit_pathspec_new(emacs_env *env, emacs_value _pathspecs)
{
    git_strarray pathspecs;
    if (!egit_strarray_from_list(&pathspecs, env, _pathspecs)) {
        return esym_nil;
    }

    git_pathspec* spec = NULL;
    int retval = git_pathspec_new(&spec, &pathspecs);
    egit_strarray_dispose(&pathspecs);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_PATHSPEC, spec, NULL);
}
