#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"

static void extract_options_flags(emacs_env *env, emacs_value eflags, uint32_t *flags)
{
    *flags = 0;
    while (env->is_not_nil(env, eflags)) {
        emacs_value head = em_car(env, eflags);
        if (env->eq(env, head, em_first_parent)) {
            *flags |= GIT_BLAME_FIRST_PARENT;
        }
        eflags = em_cdr(env, eflags);
    }
}

static emacs_value extract_options(emacs_env *env, emacs_value eopts, git_blame_options *opts)
{
    emacs_value flags =
        em_cdr(env, em_assq(env, em_flags, eopts));
    emacs_value min_match_characters =
        em_cdr(env, em_assq(env, em_min_match_characters, eopts));
    emacs_value newest_commit =
        em_cdr(env, em_assq(env, em_newest_commit, eopts));
    emacs_value oldest_commit =
        em_cdr(env, em_assq(env, em_oldest_commit, eopts));
    emacs_value min_line =
        em_cdr(env, em_assq(env, em_min_line, eopts));
    emacs_value max_line =
        em_cdr(env, em_assq(env, em_max_line, eopts));

    if (env->is_not_nil(env, flags)) {
        extract_options_flags(env, flags, &opts->flags);
    }
    if (env->is_not_nil(env, min_match_characters)) {
        opts->min_match_characters =
            (uint16_t) env->extract_integer(env, min_match_characters);
    }
    if (env->is_not_nil(env, newest_commit)) {
        EGIT_EXTRACT_OID(newest_commit, opts->newest_commit);
    }
    if (env->is_not_nil(env, oldest_commit)) {
        EGIT_EXTRACT_OID(oldest_commit, opts->oldest_commit);
    }
    if (env->is_not_nil(env, min_line)) {
        opts->min_line = (size_t) env->extract_integer(env, min_line);
    }
    if (env->is_not_nil(env, max_line)) {
        opts->max_line = (size_t) env->extract_integer(env, max_line);
    }

    return em_t;
}

EGIT_DOC(blame_file, "REPOSITORY PATH &optional OPTIONS",
         "Return the BLAME object for the given file PATH.");
emacs_value egit_blame_file(emacs_env *env, emacs_value _repo, emacs_value _path, emacs_value _options)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_path);

    git_repository *repo = EGIT_EXTRACT(_repo);

    git_blame_options opts;
    int retval = git_blame_init_options(&opts, GIT_BLAME_OPTIONS_VERSION);
    EGIT_CHECK_ERROR(retval);
    if (env->is_not_nil(env, _options)) {
        if (!env->is_not_nil(env, extract_options(env, _options, &opts))) {
            // We can fail if an invalid OID is specified in options.
            return em_nil;
        }
    }

    char *path = EM_EXTRACT_STRING(_path);

    git_blame *blame = NULL;
    retval = git_blame_file(&blame, repo, path, &opts);
    free(path);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_BLAME, blame, NULL);
}

EGIT_DOC(blame_get_hunk_byindex, "BLAME N", "Return the Nth hunk of BLAME.");
emacs_value egit_blame_get_hunk_byindex(emacs_env *env, emacs_value _blame, emacs_value _index)
{
    EGIT_ASSERT_BLAME(_blame);
    EM_ASSERT_INTEGER(_index);

    git_blame *blame = EGIT_EXTRACT(_blame);
    uint32_t index = (uint32_t) env->extract_integer(env, _index);

    const git_blame_hunk *hunk = git_blame_get_hunk_byindex(blame, index);
    if (!hunk) {
        em_signal_args_out_of_range(env, index);
        return em_nil;
    }

    return egit_wrap(env, EGIT_BLAME_HUNK, hunk, EM_EXTRACT_USER_PTR(_blame));
}

EGIT_DOC(blame_get_hunk_byline, "BLAME LINE", "Return the hunk from BLAME at the given LINE.");
emacs_value egit_blame_get_hunk_byline(emacs_env *env, emacs_value _blame, emacs_value _line)
{
    EGIT_ASSERT_BLAME(_blame);
    EM_ASSERT_INTEGER(_line);

    git_blame *blame = EGIT_EXTRACT(_blame);
    size_t line = (size_t) env->extract_integer(env, _line);

    const git_blame_hunk *hunk = git_blame_get_hunk_byline(blame, line);
    if (!hunk) {
        em_signal_args_out_of_range(env, line);
        return em_nil;
    }

    return egit_wrap(env, EGIT_BLAME_HUNK, hunk, EM_EXTRACT_USER_PTR(_blame));
}

EGIT_DOC(blame_get_hunk_count, "BLAME", "Return the number of HUNKS in the given BLAME.");
emacs_value egit_blame_get_hunk_count(emacs_env *env, emacs_value _blame)
{
    EGIT_ASSERT_BLAME(_blame);
    git_blame *blame = EGIT_EXTRACT(_blame);

    uint32_t count = git_blame_get_hunk_count(blame);
    return env->make_integer(env, count);
}


// =============================================================================
// Getters - blame hunk

EGIT_DOC(blame_hunk_commit_id, "BLAME-HUNK &optional ORIG",
         "Get the OID of the commit where the last change was made.\n"
         "If ORIG is non-nil, get instead the OID of the commit where the hunk was found.\n"
         "This will usually be the same commit.");
emacs_value egit_blame_hunk_commit_id(emacs_env *env, emacs_value _hunk, emacs_value orig)
{
    EGIT_ASSERT_BLAME_HUNK(_hunk);
    git_blame_hunk *hunk = EGIT_EXTRACT(_hunk);
    const char *oid_s = git_oid_tostr_s(
        EM_EXTRACT_BOOLEAN(orig) ? &hunk->final_commit_id : &hunk->orig_commit_id
    );
    return env->make_string(env, oid_s, strlen(oid_s));
}

EGIT_DOC(blame_hunk_lines, "BLAME-HUNK", "Get the total number of lines in BLAME-HUNK.");
emacs_value egit_blame_hunk_lines(emacs_env *env, emacs_value _hunk)
{
    EGIT_ASSERT_BLAME_HUNK(_hunk);
    git_blame_hunk *hunk = EGIT_EXTRACT(_hunk);
    return env->make_integer(env, hunk->lines_in_hunk);
}

EGIT_DOC(blame_hunk_orig_path, "BLAME-HUNK",
         "Get the path of the file associated with BLAME-HUNK in the commit named by\n"
         "(libgit-blame-hunk-commit-id BLAME-HUNK t).");
emacs_value egit_blame_hunk_orig_path(emacs_env *env, emacs_value _hunk)
{
    EGIT_ASSERT_BLAME_HUNK(_hunk);
    git_blame_hunk *hunk = EGIT_EXTRACT(_hunk);
    return env->make_string(env, hunk->orig_path, strlen(hunk->orig_path));
}

EGIT_DOC(blame_hunk_signature, "BLAME-HUNK &optional ORIG",
         "Get the author of the change represented by BLAME-HUNK.\n"
         "If ORIG is non-nil, instead get the author of the commit named by\n"
         "(libgit-blame-hunk-commit-id BLAME-HUNK t).");
emacs_value egit_blame_hunk_signature(emacs_env *env, emacs_value _hunk, emacs_value orig)
{
    EGIT_ASSERT_BLAME_HUNK(_hunk);
    git_blame_hunk *hunk = EGIT_EXTRACT(_hunk);
    git_signature *sig = EM_EXTRACT_BOOLEAN(orig) ? hunk->final_signature : hunk->orig_signature;
    git_signature *ret;
    int retval = git_signature_dup(&ret, sig);
    EGIT_CHECK_ERROR(retval);
    return egit_wrap(env, EGIT_SIGNATURE, ret, NULL);
}

EGIT_DOC(blame_hunk_start_line_number, "BLAME-HUNK &optional ORIG",
         "Get the line number where this hunk begins in the final version of the file.\n"
         "If ORIG is non-nil, instead get the line number from the file named by\n"
         "(libgit-blame-hunk-orig-path BLAME-HUNK) in the commit named by\n"
         "(libgit-blame-hunk-commit-id BLAME-HUNK t).");
emacs_value egit_blame_hunk_start_line_number(emacs_env *env, emacs_value _hunk, emacs_value orig)
{
    EGIT_ASSERT_BLAME_HUNK(_hunk);
    git_blame_hunk *hunk = EGIT_EXTRACT(_hunk);
    return env->make_integer(
        env, EM_EXTRACT_BOOLEAN(orig) ? hunk->final_start_line_number : hunk->orig_start_line_number
    );
}
