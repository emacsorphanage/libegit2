(ert-deftest ignore ()
  (with-temp-dir path
    (init)
    (let ((repo (libgit-repository-open path)))
      (libgit-ignore-add-rule repo "testrule")
      (should (eql t (libgit-ignore-path-ignored-p repo "testrule")))
      (libgit-ignore-clear-internal-rules repo)
      (should (eql nil (libgit-ignore-path-ignored-p repo "testrule"))))))
