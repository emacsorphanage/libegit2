#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-describe.h"


// =============================================================================
// Helpers - Describe Options

static emacs_value egit_describe_options_parse(
    emacs_env *env, emacs_value alist,
    git_describe_options *dopts,
    git_describe_format_options *fopts)
{
    int retval = git_describe_init_options(dopts, GIT_DESCRIBE_OPTIONS_VERSION);
    EGIT_CHECK_ERROR(retval);

    retval = git_describe_init_format_options(fopts, GIT_DESCRIBE_FORMAT_OPTIONS_VERSION);
    EGIT_CHECK_ERROR(retval);

    emacs_value pattern = esym_nil;
    emacs_value dirty_suffix = esym_nil;

    // Main loop through the options alist
    {
        emacs_value car, cdr;
        EM_DOLIST(option, alist, options);
        EM_ASSERT_CONS(option);

        car = em_car(env, option);
        cdr = em_cdr(env, option);

        if (EM_EQ(car, esym_max_candidates_tags)) {
            EM_ASSERT_INTEGER(cdr);
            dopts->max_candidates_tags = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EM_EQ(car, esym_strategy)) {
            if (!em_findsym_describe_strategy(&dopts->describe_strategy, env, cdr, true))
                return esym_nil;
        }
        else if (EM_EQ(car, esym_pattern)) {
            EM_ASSERT_STRING(cdr);
            pattern = cdr;
        }
        else if (EM_EQ(car, esym_only_follow_first_parent))
            dopts->only_follow_first_parent = EM_EXTRACT_BOOLEAN(cdr);
        else if (EM_EQ(car, esym_show_commit_oid_as_fallback))
            dopts->show_commit_oid_as_fallback = EM_EXTRACT_BOOLEAN(cdr);
        else if (EM_EQ(car, esym_abbreviated_size)) {
            EM_ASSERT_INTEGER(cdr);
            fopts->abbreviated_size = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EM_EQ(car, esym_always_use_long_format))
            fopts->always_use_long_format = EM_EXTRACT_BOOLEAN(cdr);
        else if (EM_EQ(car, esym_dirty_suffix)) {
            EM_ASSERT_STRING(cdr);
            dirty_suffix = cdr;
        }

        EM_DOLIST_END(options);
    }

    if (EM_EXTRACT_BOOLEAN(pattern))
        dopts->pattern = EM_EXTRACT_STRING(pattern);
    if (EM_EXTRACT_BOOLEAN(dirty_suffix))
        fopts->dirty_suffix = EM_EXTRACT_STRING(dirty_suffix);

    return esym_nil;
}

static void egit_describe_options_release(
    git_describe_options *dopts,
    git_describe_format_options *fopts)
{
    if (dopts)
        free((char*) dopts->pattern);
    if (fopts)
        free((char*) fopts->dirty_suffix);
}


// =============================================================================
// Constructors

EGIT_DOC(describe_commit, "COMMITTISH &optional OPTS",
         "Describe COMMITTISH and return as string.\n"
         "OPTS is an optional alist of options. The allowed keys are:\n"
         "- `max-candidates-tags': default 10\n"
         "- `describe-strategy': nil (default), `tags' or `all'\n"
         "- `pattern': a string\n"
         "- `only-follow-first-parent': if non-nil, only walk down the\n"
         "     first-parent ancestry when calculating distance\n"
         "- `show-commit-oid-as-fallback': show the full ID of the commit\n"
         "     as a fallback if no matching tag or reference is found\n"
         "- `abbreviated-size': default 7\n"
         "- `always-use-long-format': if non-nil, use the long format even\n"
         "     when a shorter name could be used\n"
         "- `dirty-suffix': if the workdir is dirty, string to be appended\n"
         "     to the description.");
emacs_value egit_describe_commit(emacs_env *env, emacs_value _committish, emacs_value opts)
{
    EGIT_ASSERT_OBJECT(_committish);
    git_object *committish = EGIT_EXTRACT(_committish);

    git_describe_options dopts;
    git_describe_format_options fopts;
    egit_describe_options_parse(env, opts, &dopts, &fopts);
    EM_RETURN_NIL_IF_NLE();

    git_describe_result *result;
    int retval = git_describe_commit(&result, committish, &dopts);
    egit_describe_options_release(&dopts, NULL);
    EGIT_CHECK_ERROR(retval);

    git_buf buf = {0};
    retval = git_describe_format(&buf, result, &fopts);
    egit_describe_options_release(NULL, &fopts);
    git_describe_result_free(result);
    EGIT_CHECK_ERROR(retval);

    EGIT_RET_BUF_AS_STRING(buf);
}

EGIT_DOC(describe_workdir, "REPO &optional OPTS",
         "Describe HEAD and the working directory, and return as string.\n"
         "See `libgit-describe-commit' for a description of OPTS.");
emacs_value egit_describe_workdir(emacs_env *env, emacs_value _repo, emacs_value opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    git_describe_options dopts;
    git_describe_format_options fopts;
    egit_describe_options_parse(env, opts, &dopts, &fopts);
    EM_RETURN_NIL_IF_NLE();

    git_describe_result *result;
    int retval = git_describe_workdir(&result, repo, &dopts);
    egit_describe_options_release(&dopts, NULL);
    EGIT_CHECK_ERROR(retval);

    git_buf buf = {0};
    retval = git_describe_format(&buf, result, &fopts);
    egit_describe_options_release(NULL, &fopts);
    git_describe_result_free(result);
    EGIT_CHECK_ERROR(retval);

    EGIT_RET_BUF_AS_STRING(buf);
}
