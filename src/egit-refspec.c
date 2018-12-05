#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-refspec.h"


// =============================================================================
// Getters

EGIT_DOC(refspec_direction, "REFSPEC",
         "Get the direction of REFSPEC.\n"
         "This is either `fetch' or `push'.");
emacs_value egit_refspec_direction(emacs_env *env, emacs_value _refspec)
{
    EGIT_ASSERT_REFSPEC(_refspec);
    git_refspec *refspec = EGIT_EXTRACT(_refspec);
    git_direction dir = git_refspec_direction(refspec);
    return em_findenum_direction(dir);
}

EGIT_DOC(refspec_dst, "REFSPEC", "Get the destination specifier of REFSPEC.");
emacs_value egit_refspec_dst(emacs_env *env, emacs_value _refspec)
{
    EGIT_ASSERT_REFSPEC(_refspec);
    git_refspec *refspec = EGIT_EXTRACT(_refspec);
    const char *dst = git_refspec_dst(refspec);
    return EM_STRING(dst);
}

EGIT_DOC(refspec_src, "REFSPEC", "Get the source specifier of REFSPEC.");
emacs_value egit_refspec_src(emacs_env *env, emacs_value _refspec)
{
    EGIT_ASSERT_REFSPEC(_refspec);
    git_refspec *refspec = EGIT_EXTRACT(_refspec);
    const char *src = git_refspec_src(refspec);
    return EM_STRING(src);
}

EGIT_DOC(refspec_string, "REFSPEC", "Get the string representation of REFSPEC.");
emacs_value egit_refspec_string(emacs_env *env, emacs_value _refspec)
{
    EGIT_ASSERT_REFSPEC(_refspec);
    git_refspec *refspec = EGIT_EXTRACT(_refspec);
    const char *str = git_refspec_string(refspec);
    return EM_STRING(str);
}


// =============================================================================
// Predicates

EGIT_DOC(refspec_dst_matches_p, "REFSPEC REFNAME",
         "Non-nil if REFNAME matches the destination descriptor of REFSPEC.");
emacs_value egit_refspec_dst_matches_p(emacs_env *env, emacs_value _refspec, emacs_value _refname)
{
    EGIT_ASSERT_REFSPEC(_refspec);
    EM_ASSERT_STRING(_refname);
    git_refspec *refspec = EGIT_EXTRACT(_refspec);
    char *refname = EM_EXTRACT_STRING(_refname);
    int ret = git_refspec_dst_matches(refspec, refname);
    free(refname);
    return ret ? esym_t : esym_nil;
}

EGIT_DOC(refspec_force_p, "REFSPEC", "Non-nil if the force setting of REFSPEC is set.");
emacs_value egit_refspec_force_p(emacs_env *env, emacs_value _refspec)
{
    EGIT_ASSERT_REFSPEC(_refspec);
    git_refspec *refspec = EGIT_EXTRACT(_refspec);
    return git_refspec_force(refspec) ? esym_t : esym_nil;
}

EGIT_DOC(refspec_src_matches_p, "REFSPEC REFNAME",
         "Non-nil if REFNAME matches the source descriptor of REFSPEC.");
emacs_value egit_refspec_src_matches_p(emacs_env *env, emacs_value _refspec, emacs_value _refname)
{
    EGIT_ASSERT_REFSPEC(_refspec);
    EM_ASSERT_STRING(_refname);
    git_refspec *refspec = EGIT_EXTRACT(_refspec);
    char *refname = EM_EXTRACT_STRING(_refname);
    int ret = git_refspec_src_matches(refspec, refname);
    free(refname);
    return ret ? esym_t : esym_nil;
}
