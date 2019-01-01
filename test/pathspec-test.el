(ert-deftest pathspec-new ()
  (let* ((pathspec (libgit-pathspec-new '("file-a"))))
      (should pathspec)
      (should (libgit-pathspec-p pathspec))))
