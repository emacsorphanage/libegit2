#include <string.h>
#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-options.h"
#include "egit-repository.h"
#include "egit-util.h"

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
    if (!em_findsym_reset(&type, env, _type, true))
        return esym_nil;

    git_checkout_options opts;
    egit_checkout_options_parse(env, _opts, &opts);
    EM_RETURN_NIL_IF_NLE();

    int retval = git_reset(repo, obj, type, &opts);
    egit_checkout_options_release(&opts);
    EGIT_CHECK_ERROR(retval);

    return esym_t;
}

EGIT_DOC(reset_from_annotated, "REPOSITORY ANNOTATED-COMMIT TYPE &optional CHECKOUT-OPTIONS",
         "Sets the current head to the specified ANNOTATED-COMMIT and optionally"
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
emacs_value egit_reset_from_annotated(emacs_env *env, emacs_value _repo,
				      emacs_value _ann, emacs_value _type,
				      emacs_value _opts)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    EGIT_ASSERT_ANNOTATED_COMMIT(_ann);

    git_repository *repo = EGIT_EXTRACT(_repo);
    git_annotated_commit *ann = EGIT_EXTRACT(_ann);

    git_reset_t type;
    if (!em_findsym_reset(&type, env, _type, true))
        return esym_nil;

    git_checkout_options opts;
    egit_checkout_options_parse(env, _opts, &opts);
    EM_RETURN_NIL_IF_NLE();

    int retval = git_reset_from_annotated(repo, ann, type, &opts);
    egit_checkout_options_release(&opts);
    EGIT_CHECK_ERROR(retval);

    return esym_t;
}

EGIT_DOC(reset_default, "REPOSITORY TARGET PATHSPECS",
	 "Updates some entries in the index from the target commit tree.\n"
	 "\n"
	 "The scope of the updated entries is determined by the paths"
	 "being passed in the PATHSPECS list.\n"
	 "\n"
	 "Passing a nil TARGET will result in removing entries in the index"
	 "matching the provided pathspecs.");
emacs_value egit_reset_default(emacs_env *env, emacs_value _repo,
			       emacs_value _target, emacs_value _pathspecs)
{
    EGIT_ASSERT_REPOSITORY(_repo);
    git_repository *repo = EGIT_EXTRACT(_repo);

    git_object *target = NULL;
    if (EM_EXTRACT_BOOLEAN(_target)) {
	EGIT_ASSERT_OBJECT(_target);
	target = EGIT_EXTRACT(_target);
    }

    git_strarray pathspecs;
    if (!egit_strarray_from_list(&pathspecs, env, _pathspecs)) {
	return esym_nil;
    }

    int retval = git_reset_default(repo, target, &pathspecs);
    egit_strarray_dispose(&pathspecs);
    EGIT_CHECK_ERROR(retval);

    return esym_t;
}
