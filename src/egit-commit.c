#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"

EGIT_DOC(commit_create, "REPO &optional UPDATE-REF AUTHOR COMMITTER MESSAGE-ENCODING MESSAGE TREE PARENT-COUNT parents", "Create new commit in the repository from a list of git_object pointers");

emacs_value egit_commit_create(emacs_env *env, emacs_value _repo, emacs_value _update_ref, emacs_value _author, emacs_value _committer, emacs_value _message_encoding, emacs_value _message, emacs_value _tree, emacs_value _parents)
{
 EGIT_ASSERT_REPOSITORY(_repo);
 EGIT_ASSERT_REFERENCE(_update_ref);
 EGIT_ASSERT_SIGNATURE(_author);
 EGIT_ASSERT_SIGNATURE(_committer);
 EGIT_ASSERT_STRING_OR_NIL(_message_encoding);
 EGIT_ASSERT_STRING_OR_NIL(_message);
 EGIT_ASSERT_TREE(_tree);


 
 git_repository *repo = EGIT_EXTRACT(_repo);
 char *update_ref = EGIT_EXTRACT_STRING_OR_NULL(_update_ref);
 git_signature *author = EGIT_EXTRACT(_author);
 git_signature *committer = EGIT_EXTRACT(_committer);
 char *message_encoding = EGIT_EXTRACT_STRING_OR_NULL(_message_encoding);
 char *message = EGIT_EXTRACT_STRING_OR_NULL(_message);
 git_tree *tree = EGIT_EXTRACT(_tree);
 
}
