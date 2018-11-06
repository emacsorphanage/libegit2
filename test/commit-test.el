(ert-deftest commit-lookup ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let* ((repo (libgit-repository-open path))
           (id (libgit-reference-name-to-id repo "HEAD")))
      (should (libgit-commit-p (libgit-commit-lookup repo id)))
      (should-error (libgit-commit-lookup repo "test") :type 'giterr))))
