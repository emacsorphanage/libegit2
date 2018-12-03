#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"


static emacs_value extract_options(emacs_env *env, emacs_value eopts, git_blame_options *opts)
{
    int retval = git_blame_init_options(opts, GIT_BLAME_OPTIONS_VERSION);
    EGIT_CHECK_ERROR(retval);

    {
        emacs_value car, cdr;
        EM_DOLIST(option, eopts, loop);
        EM_ASSERT_CONS(option);

        car = em_car(env, option);
        cdr = em_cdr(env, option);

        if (EM_EQ(car, esym_first_parent))
            EGIT_SET_BIT(opts->flags, GIT_BLAME_FIRST_PARENT, cdr);

        /* According to libgit2 documentation, the min_match_characters
         * setting only takes effect if GIT_BLAME_TRACK_COPIES_* flags
         * are set, but all of those are marked as not implemented in
         * the source code.
        else if (EM_EQ(car, esym_min_match_characters)) {
            EM_ASSERT_INTEGER(cdr);
            opts->min_match_characters = EM_EXTRACT_INTEGER(cdr);
        }
        */

        else if (EM_EQ(car, esym_newest_commit)) {
            EM_ASSERT_STRING(cdr);
            EGIT_EXTRACT_OID(cdr, opts->newest_commit);
        }
        else if (EM_EQ(car, esym_oldest_commit)) {
            EM_ASSERT_STRING(cdr);
            EGIT_EXTRACT_OID(cdr, opts->oldest_commit);
        }
        else if (EM_EQ(car, esym_min_line)) {
            EM_ASSERT_INTEGER(cdr);
            opts->min_line = EM_EXTRACT_INTEGER(cdr);
        }
        else if (EM_EQ(car, esym_max_line)) {
            EM_ASSERT_INTEGER(cdr);
            opts->max_line = EM_EXTRACT_INTEGER(cdr);
        }
        else {
            em_signal_wrong_value(env, car);
            return esym_nil;
        }

        EM_DOLIST_END(loop);
    }

    return esym_t;
}

EGIT_DOC(blame_file, "REPOSITORY PATH &optional OPTIONS",
         "Return the BLAME object for the given file PATH.\n"
         "OPTIONS is an alist with the following allowed keys:\n"
         "- `first-parent': if non-nil, restrict the search of commits to\n"
         "     those reachable by first parents\n"
         "- `newest-commit': the ID of the newest commit to consider (default HEAD)\n"
         "- `oldest-commit': the ID of the oldest commit to consider, by default\n"
         "     the search stops on the first commit without a parent\n"
         "- `min-line': the first line in the file to blame (default 1)\n"
         "- `max-line': the last line in the file to blame (default last line)");
emacs_value egit_blame_file(emacs_env *env, emacs_value _repo, emacs_value _path, emacs_value options)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EM_ASSERT_STRING(_path);

    git_repository *repo = EGIT_EXTRACT(_repo);

    git_blame_options opts;
    extract_options(env, options, &opts);
    EM_RETURN_NIL_IF_NLE();

    char *path = EM_EXTRACT_STRING(_path);

    git_blame *blame = NULL;
    int retval = git_blame_file(&blame, repo, path, &opts);
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
        return esym_nil;
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
        return esym_nil;
    }

    return egit_wrap(env, EGIT_BLAME_HUNK, hunk, EM_EXTRACT_USER_PTR(_blame));
}

EGIT_DOC(blame_get_hunk_count, "BLAME", "Return the number of HUNKS in the given BLAME.");
emacs_value egit_blame_get_hunk_count(emacs_env *env, emacs_value _blame)
{
    EGIT_ASSERT_BLAME(_blame);
    git_blame *blame = EGIT_EXTRACT(_blame);

    uint32_t count = git_blame_get_hunk_count(blame);
    return EM_INTEGER(count);
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
    return EM_STRING(oid_s);
}

EGIT_DOC(blame_hunk_lines, "BLAME-HUNK", "Get the total number of lines in BLAME-HUNK.");
emacs_value egit_blame_hunk_lines(emacs_env *env, emacs_value _hunk)
{
    EGIT_ASSERT_BLAME_HUNK(_hunk);
    git_blame_hunk *hunk = EGIT_EXTRACT(_hunk);
    return EM_INTEGER(hunk->lines_in_hunk);
}

EGIT_DOC(blame_hunk_orig_path, "BLAME-HUNK",
         "Get the path of the file associated with BLAME-HUNK in the commit named by\n"
         "(libgit-blame-hunk-commit-id BLAME-HUNK t).");
emacs_value egit_blame_hunk_orig_path(emacs_env *env, emacs_value _hunk)
{
    EGIT_ASSERT_BLAME_HUNK(_hunk);
    git_blame_hunk *hunk = EGIT_EXTRACT(_hunk);
    return EM_STRING(hunk->orig_path);
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
    return EM_INTEGER(
        EM_EXTRACT_BOOLEAN(orig) ? hunk->final_start_line_number : hunk->orig_start_line_number
    );
}
