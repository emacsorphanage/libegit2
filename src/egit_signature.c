#include "git2.h"

#include "egit.h"
#include "interface.h"

EGIT_DOC(signature_default, "REPO", "Create a new action signature with default user and now timestamp");

emacs_value egit_signature_default(emacs_env *env, emacs_value _repo)
{
  EGIT_ASSERT_REPOSITORY(_repo);

  git_repository *repo = EGIT_EXTRACT(_repo);

  git_signature *signature;
  int retval;
  retval = git_signature_default(&signature, repo);
  EGIT_CHECK_ERROR(retval);

  return egit_wrap(env, EGIT_SIGNATURE, signature);
}
