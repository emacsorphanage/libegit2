(ert-deftest branch-create ()
  (with-temp-dir path
                 (init)
                 (commit-change "test" "content")
                 (let ((repo (libgit-repository-open path)))
                   (should (libgit-branch-create repo "new-branch" "HEAD"))
                   (should-error (libgit-branch-create repo "new-branch" "HEAD"))))
  (with-temp-dir path
                 (init)
                 (commit-change "test" "content")
                 (run "git" "branch" "second")
                 (run "git" "checkout" "second")
                 (commit-change "test2" "content2")
                 (let ((repo (libgit-repository-open path)))
                   (should-error (libgit-branch-create repo "master" "second"))
                   (should (libgit-branch-create repo "master" "second" t)))))

