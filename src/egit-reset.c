#include <string.h>
#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-options.h"
#include "egit-repository.h"

#include "egit-reset.h"

EGIT_DOC(reset, "REPOSITORY OBJECT TYPE &optional CHECKOUT-OPTIONS",
         "Sets the current head to the specified commit OBJECT and optionally"
	 " resets the index and working tree to match.\n"
	 "TYPE must be a symbol that determines how the reset will be done:\n"
 	 "\n"
	 "  - `soft', the Head will be moved to the OBJECT commit\n"
	 "\n"
	 "  - `mixed' will trigger a \"soft\" reset, plus the index will be "
	 "replaced with the content of the commit tree\n"
	 "\n"
	 "  - `hard' will trigger a \"mixed\" reset and the working directory "
	 "will be replaced with the content of the index."
	 " (Untracked and ignored files will be left alone, however.)");
emacs_value egit_reset(emacs_env *env, emacs_value _repo, emacs_value _obj,
		       emacs_value _type, emacs_value _opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_OBJECT(_obj);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_object *obj = EGIT_EXTRACT(_obj);

    git_reset_t type;
    if (EM_EQ(_type, em_soft))
        type = GIT_RESET_SOFT;
    else if (EM_EQ(_type, em_mixed))
        type = GIT_RESET_MIXED;
    else if (EM_EQ(_type, em_hard))
        type = GIT_RESET_HARD;
    else {
        em_signal_wrong_value(env, _type);
        return em_nil;
    }

    git_checkout_options opts;
    egit_checkout_options_parse(env, _opts, &opts);
    EM_RETURN_NIL_IF_NLE();

    int retval = git_reset(repo, obj, type, &opts);
    egit_checkout_options_release(&opts);
    EGIT_CHECK_ERROR(retval);

    return em_t;
}
