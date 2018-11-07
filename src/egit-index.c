#include <string.h>

#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-index.h"


// =============================================================================
// Helpers

static git_index_entry *egit_index_entry_dup(const git_index_entry *index)
{
    git_index_entry *new = (git_index_entry*) malloc(sizeof(git_index_entry));
    memcpy(new, index, sizeof(git_index_entry));
    new->path = strdup(index->path);
    return new;
}


// =============================================================================
// Getters

EGIT_DOC(index_entrycount, "INDEX", "Get the number of entries in INDEX.");
emacs_value egit_index_entrycount(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    size_t count = git_index_entrycount(index);
    return env->make_integer(env, count);
}

EGIT_DOC(index_get_byindex, "INDEX N", "Get the Nth entry in INDEX.");
emacs_value egit_index_get_byindex(emacs_env *env, emacs_value _index, emacs_value _n)
{
    EGIT_ASSERT_INDEX(_index);
    EGIT_ASSERT_INTEGER(_n);
    git_index *index = EGIT_EXTRACT(_index);
    intmax_t n = EGIT_EXTRACT_INTEGER(_n);
    const git_index_entry *entry = git_index_get_byindex(index, n);
    if (!entry) {
        em_signal_args_out_of_range(env, n);
        return em_nil;
    }
    return egit_wrap(env, EGIT_INDEX_ENTRY, egit_index_entry_dup(entry));
}

EGIT_DOC(index_owner, "INDEX", "Return the repository associated with INDEX.");
emacs_value egit_index_owner(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    git_repository *repo = git_index_owner(index);
    return egit_wrap_repository(env, repo);
}
