#include <string.h>

#include "git2.h"

#include "egit.h"
#include "egit-util.h"
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

static emacs_value extract_flags(int32_t *out, emacs_env* env, emacs_value _flags) {
    {
        EM_DOLIST(_flag, _flags, flags_label);
        git_pathspec_flag_t flag = 0;
        em_findsym_pathspec_flag(&flag, env, _flag, true);
        *out |= flag;
        EM_DOLIST_END(flags_label);
    }
    return esym_t;
}

EGIT_DOC(pathspec_matches_path, "PATHSPEC FLAGS PATH",
         "Try to match a PATH against a PATHSPEC.\n"
         "\n"
         "FLAGS should be nil or a list with the following symbols:\n"
         "  - ignore-case: forces match to ignore case\n"
         "  - use-case: forces case sensitive match\n"
         "  - no-glob: disables glob patterns and just uses simple string "
         "comparison for matching\n"
         "\n"
         "Unlike most of the other pathspec matching functions, this will not "
         "fall back on the native case-sensitivity for your platform. "
         "You must explicitly pass flags to control case sensitivity or else "
         "this will fall back on being case sensitive.");
emacs_value egit_pathspec_matches_path(emacs_env *env, emacs_value _pathspec,
                                       emacs_value _flags, emacs_value _path)
{
    EGIT_ASSERT_PATHSPEC(_pathspec);
    EM_ASSERT_STRING(_path);

    git_pathspec *pathspec = EGIT_EXTRACT(_pathspec);
    int32_t flags = 0;
    extract_flags(&flags, env, _flags);
    char *path = EM_EXTRACT_STRING(_path);

    bool retval = git_pathspec_matches_path(pathspec, flags, path);
    free(path);
    return retval ? esym_t : esym_nil;
}
