#include <string.h>
#include <stdio.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-libgit2.h"


EGIT_DOC(libgit2_feature_p, "FEATURE",
         "Check if libgit2 was compiled with FEATURE.\n"
         "FEATURE may be any of the symbols `threads', `https' or `ssh'.");
emacs_value egit_libgit2_feature_p(emacs_env *env, emacs_value feature)
{
    git_feature_t features = git_libgit2_features();
    emacs_value retval;
    em_checkflag_feature(&retval, env, feature, features, true);
    return retval;
}

EGIT_DOC(libgit2_version, "", "Get the version of the underlying libgit2.");
emacs_value egit_libgit2_version(emacs_env *env)
{
    int major, minor, rev;
    git_libgit2_version(&major, &minor, &rev);

    int nchars = snprintf(NULL, 0, "%d.%d.%d", major, minor, rev);
    char buf[nchars];
    snprintf(buf, 100, "%d.%d.%d", major, minor, rev);
    return EM_STRING(buf);
}
