#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-transaction.h"


EGIT_DOC(transaction_commit, "TRANS", "Commit the changes made in TRANS.");
emacs_value egit_transaction_commit(emacs_env *env, emacs_value _trans)
{
    EGIT_ASSERT_TRANSACTION(_trans);
    git_transaction *trans = EGIT_EXTRACT(_trans);
    int retval = git_transaction_commit(trans);
    EGIT_CHECK_ERROR(retval);
    return esym_nil;
}
