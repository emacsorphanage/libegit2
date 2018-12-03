#include "git2.h"

#include "egit.h"
#include "interface.h"

EGIT_DOC(add_rule, "REPO RULES",
         "Add ignore rules listed in RULES to libgit2s internal list of ignore \
         rules for REPO. These rules are merged with any rules listed in a \
         .gitignore file in the repo. Rules are separated by newlines.");
emacs_value egit_add_rule(emacs_env *env, emacs_value _repo, emacs_value _rules)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_rules);

    git_repository *repo = EGIT_EXTRACT(_repo);
    int retval;
    {
        char *rules = EM_EXTRACT_STRING(_rules);
        retval = git_ignore_add_rule(repo, rules);
        free(rules);
    }
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(clear_internal_rules, "REPO",
         "Clear internal ignore rules. Note that this function can only be used \
         to clear files added to the internal list of ignore rules, not ones \
         listed in a .gitignore file.");
emacs_value egit_clear_internal_rules(emacs_env *env, emacs_value _repo)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    int retval = git_ignore_clear_internal_rules(repo);
    EGIT_CHECK_ERROR(retval);

    return esym_nil;
}

EGIT_DOC(path_ignored_p, "REPO PATH", "Check if PATH is ignored");
emacs_value egit_path_ignored_p(emacs_env *env, emacs_value _repo, emacs_value _path)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_path);

    git_repository *repo = EGIT_EXTRACT(_repo);

    int out;
    int retval;
    {
        char *path = EM_EXTRACT_STRING(_path)
        retval = git_ignore_path_is_ignored(&out, repo, path);
        free(path);
    }
    EGIT_CHECK_ERROR(retval);
    return out ? esym_t : esym_nil;
}
