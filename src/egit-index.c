#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-index.h"


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

EGIT_DOC(index_owner, "INDEX", "Return the repository associated with INDEX.");
emacs_value egit_index_owner(emacs_env *env, emacs_value _index)
{
    EGIT_ASSERT_INDEX(_index);
    git_index *index = EGIT_EXTRACT(_index);
    git_repository *repo = git_index_owner(index);
    return egit_wrap_repository(env, repo);
}
