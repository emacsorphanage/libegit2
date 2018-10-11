#include "git2.h"

#include "egit.h"
#include "interface.h"
#include "egit-repository.h"

int get_parent_count(emacs_env *env, emacs_value _parents);

EGIT_DOC(commit_create, "REPO &optional UPDATE-REF AUTHOR COMMITTER MESSAGE-ENCODING MESSAGE TREE PARENTS", "Create new commit in the repository from a list of git_object pointers");

emacs_value egit_commit_create(emacs_env *env, emacs_value _repo, emacs_value _update_ref, emacs_value _author, emacs_value _committer, emacs_value _message_encoding, emacs_value _message, emacs_value _tree, emacs_value _parents)
{
 EGIT_ASSERT_REPOSITORY(_repo);
 EGIT_ASSERT_REFERENCE(_update_ref);
 EGIT_ASSERT_SIGNATURE(_author);
 EGIT_ASSERT_SIGNATURE(_committer);
 EGIT_ASSERT_STRING_OR_NIL(_message_encoding);
 EGIT_ASSERT_STRING_OR_NIL(_message);
 EGIT_ASSERT_TREE(_tree);

 int  nparents = get_parent_count(env, _parents);
 
 git_repository *repo = EGIT_EXTRACT(_repo);
 char *update_ref = EGIT_EXTRACT_STRING_OR_NULL(_update_ref);
 git_signature *author = EGIT_EXTRACT(_author);
 git_signature *committer = EGIT_EXTRACT(_committer);
 char *message_encoding = EGIT_EXTRACT_STRING_OR_NULL(_message_encoding);
 char *message = EGIT_EXTRACT_STRING_OR_NULL(_message);
 git_tree *tree = EGIT_EXTRACT(_tree);
 git_commit *parents[nparents];

 emacs_value cell = _parents;
 for (int i = 0; i < nparents; ++i) {
   emacs_value car = em_car(env, cell);
   EGIT_ASSERT_COMMIT(car);
   parents[i] = EGIT_EXTRACT(car);
   cell = em_cdr(env, cell);
 }

 int retval;
 git_oid *new_oid;
 retval = git_commit_create(new_oid, repo, update_ref, author, committer, message_encoding, message, tree, nparents, parents);
 EGIT_CHECK_ERROR(retval);
 return egit_wrap(env, EGIT_OBJECT, new_oid);
}

int get_parent_count(emacs_env *env, emacs_value _parents)
{
  int retval = 0;
  emacs_value cell = _parents;
  while(em_consp(env, cell)){
    retval++;
    cell = em_cdr(env, cell);
  }
  return retval;
}
