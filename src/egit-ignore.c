#include "git2.h"

#include "egit.h"
#include "interface.h"

EGIT_DOC(add_rule, "REPO RULES",
         "Add ignore rules listed in RULES, separated by newline to REPO");
emacs_value egit_add_rule(emacs_env *env, emacs_value _repo, emacs_value _rules)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_rules);

    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval;
    {
        char *rules = EGIT_EXTRACT_STRING(_rules);
        retval = git_ignore_add_rule(repo, rules);
        free(rules);
    }
    EGIT_CHECK_ERROR(retval);

    return env->make_integer(env, retval);
}

emacs_value egit_clear_internal_rules(emacs_env *env, emacs_value _repo)
{

}

emacs_value egit_path_ignored_p(emacs_env *env, emacs_value _repo, emacs_value _path)
{

}
