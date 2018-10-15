(ert-deftest annotated-commit-from-ref ()
  (with-temp-dir path
    (init)
    (commit-change "test" "foo")
    (let* ((repo (libgit-repository-open path))
           (ref  (libgit-reference-lookup repo "HEAD"))
           (ann  (libgit-annotated-commit-from-ref repo ref)))
      (should (libgit-annotated-commit-p ann)))))
