#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"

static void extract_options_flags(emacs_env *env,
                                  emacs_value eflags,
                                  uint32_t *flags)
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

static emacs_value extract_options(emacs_env *env,
                                   emacs_value eopts,
                                   git_blame_options *opts)
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
emacs_value egit_blame_file(emacs_env *env,
                            emacs_value _repo,
                            emacs_value _path,
                            emacs_value _options)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_STRING(_path);

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

    char *path = EGIT_EXTRACT_STRING(_path);

    git_blame *blame = NULL;
    retval = git_blame_file(&blame, repo, path, &opts);
    free(path);
    EGIT_CHECK_ERROR(retval);

    return egit_wrap(env, EGIT_BLAME, blame);
}

static
emacs_value make_object_id(emacs_env *env, const git_oid *oid)
{
    const char *oid_s = git_oid_tostr_s(oid);
    return env->make_string(env, oid_s, strlen(oid_s));
}

static
emacs_value make_blame_hunk(emacs_env *env, const git_blame_hunk *hunk)
{
    emacs_value lines_in_hunk =
        em_cons(env, em_lines_in_hunk,
                env->make_integer(env, hunk->lines_in_hunk));

    emacs_value final_commit_id =
        em_cons(env, em_final_commit_id,
                make_object_id(env, &hunk->final_commit_id));

    emacs_value final_start_line_number =
        em_cons(env, em_final_start_line_number,
                env->make_integer(env, hunk->final_start_line_number));

    // TODO: handle signature
    // git_signature *final_signature;

    emacs_value orig_commit_id =
        em_cons(env, em_orig_commit_id,
                make_object_id(env, &hunk->orig_commit_id));

    emacs_value orig_path =
        em_cons(env, em_orig_path,
                env->make_string(env, hunk->orig_path,
                                 strlen(hunk->orig_path)));

    emacs_value orig_start_line_number =
        em_cons(env, em_orig_start_line_number,
                env->make_integer(env, hunk->orig_start_line_number));

    // TODO: handle signature
    // git_signature *orig_signature;

    emacs_value boundary =
        em_cons(env, em_boundary,
                (hunk->boundary == 1) ? em_t : em_nil);

    emacs_value args[] = {
        lines_in_hunk,
        final_commit_id,
        final_start_line_number,
        orig_commit_id,
        orig_path,
        orig_start_line_number,
        boundary
    };

    return em_list(env, args, sizeof(args)/sizeof(*args));
}

EGIT_DOC(blame_get_hunk_byindex, "BLAME INDEX",
         "Return the HUNK alist with the given INDEX.");
emacs_value egit_blame_get_hunk_byindex(emacs_env *env,
                                        emacs_value _blame,
                                        emacs_value _index)
{
    EGIT_ASSERT_BLAME(_blame);

    git_blame *blame = EGIT_EXTRACT(_blame);
    uint32_t index = (uint32_t) env->extract_integer(env, _index);

    const git_blame_hunk *hunk = git_blame_get_hunk_byindex(blame, index);
    if (!hunk) {
        return em_nil;
    }

    return make_blame_hunk(env, hunk);
}

EGIT_DOC(blame_get_hunk_byline, "BLAME LINE",
         "Return the HUNK alist for the given LINE.");
emacs_value egit_blame_get_hunk_byline(emacs_env *env,
                                       emacs_value _blame,
                                       emacs_value _line)
{
    EGIT_ASSERT_BLAME(_blame);

    git_blame *blame = EGIT_EXTRACT(_blame);
    size_t line = (size_t) env->extract_integer(env, _line);

    const git_blame_hunk *hunk = git_blame_get_hunk_byline(blame, line);
    if (!hunk) {
        return em_nil;
    }

    return make_blame_hunk(env, hunk);
}

EGIT_DOC(blame_get_hunk_count, "BLAME",
         "Return the number of HUNKS in the given BLAME.");
emacs_value egit_blame_get_hunk_count(emacs_env *env,
                                      emacs_value _blame)
{
    EGIT_ASSERT_BLAME(_blame);
    git_blame *blame = EGIT_EXTRACT(_blame);

    uint32_t count = git_blame_get_hunk_count(blame);
    return env->make_integer(env, count);
}
