(ert-deftest blame-file ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let* ((repo (libgit-repository-open path))
           (blame (libgit-blame-file repo "test"))))))
