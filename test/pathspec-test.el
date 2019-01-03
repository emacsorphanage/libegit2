(ert-deftest pathspec-new ()
  (let* ((pathspec (libgit-pathspec-new '("file-a"))))
      (should pathspec)
      (should (libgit-pathspec-p pathspec))))

(ert-deftest pathspec-matches-path ()
  (let ((pathspec (libgit-pathspec-new '("file-a" "file-b"))))
    (should (libgit-pathspec-matches-path pathspec nil "file-a"))
    (should (libgit-pathspec-matches-path pathspec nil "file-b"))
    (should (not (libgit-pathspec-matches-path pathspec nil "non-existent")))

    (should (libgit-pathspec-matches-path pathspec '(ignore-case) "FiLe-A"))
    (should (not (libgit-pathspec-matches-path pathspec '(use-case) "FiLe-A"))))

  (let ((pathspec-glob (libgit-pathspec-new '("file-*"))))
    (should (libgit-pathspec-matches-path pathspec-glob nil "file-a"))
    (should (not (libgit-pathspec-matches-path pathspec-glob '(no-glob) "file-a")))

    (should (libgit-pathspec-matches-path pathspec-glob
					  '(ignore-case) "FiLe-A"))
    (should (not (libgit-pathspec-matches-path pathspec-glob
					       '(use-case) "FiLe-A")))))
