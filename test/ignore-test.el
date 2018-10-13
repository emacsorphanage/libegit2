(ert-deftest ignore ()
  (with-temp-dir path
    (init)
    (let ((repo (libgit-repository-open path)))
      (libgit-ignore-add-rule repo "testrule")
      (should (= 1 (libgit-ignore-path-ignored-p repo "testrule")))
      (libgit-ignore-clear-internal-rules repo)
      (should (= 0 (libgit-ignore-path-ignored-p repo "testrule")))))
