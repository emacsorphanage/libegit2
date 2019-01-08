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

(ert-deftest pathspec-match-workdir ()
  (with-temp-dir path
    (init)
    (commit-change "file-a" "content-a")
    (commit-change "file-b" "content-b")

    (let* ((repo (libgit-repository-open path))
           (pathspec (libgit-pathspec-new '("file-*")))
           (match-list (libgit-pathspec-match-workdir repo nil pathspec)))
      (should match-list)
      (should (= 2 (libgit-pathspec-match-list-entrycount match-list)))
      (should (string= "file-a"
                       (libgit-pathspec-match-list-entry match-list 0)))
      (should (string= "file-b"
                       (libgit-pathspec-match-list-entry match-list 1))))

    (let* ((repo (libgit-repository-open path))
           (pathspec (libgit-pathspec-new '("file-a" "file-c" "file-d")))
           (match-list (libgit-pathspec-match-workdir repo '(find-failures)
                                                      pathspec)))
      (should match-list)

      ;; Find matched files
      (should (= 1 (libgit-pathspec-match-list-entrycount match-list)))
      (should (string= "file-a"
                       (libgit-pathspec-match-list-entry match-list 0)))

      ;; Find all pathspecs that have no matches
      (should (= 2 (libgit-pathspec-match-list-failed-entrycount match-list)))
      (should (string= "file-c"
                       (libgit-pathspec-match-list-failed-entry match-list 0)))
      (should (string= "file-d"
                       (libgit-pathspec-match-list-failed-entry match-list 1))))))

(ert-deftest pathspec-match-index ()
  (with-temp-dir path
    (init)
    (write "file-a" "content-a")
    (write "file-b" "content-b")

    ;; Only file-a should match - file-b is not in an index
    (run "git" "add" "file-a")

    ;; Test with glob
    (let* ((index (libgit-repository-index (libgit-repository-open path)))
           (pathspec (libgit-pathspec-new '("file-*")))
           (match-list (libgit-pathspec-match-index index nil pathspec)))
      (should match-list)
      (should (= 1 (libgit-pathspec-match-list-entrycount match-list)))
      (should (string= "file-a"
                       (libgit-pathspec-match-list-entry match-list 0))))

    (let* ((index (libgit-repository-index (libgit-repository-open path)))
           (pathspec (libgit-pathspec-new
		      '("file-a" "file-b" "file-c" "file-d")))
           (match-list (libgit-pathspec-match-index index '(find-failures)
                                                    pathspec)))
      (should match-list)

      ;; Find matched files
      (should (= 1 (libgit-pathspec-match-list-entrycount match-list)))
      (should (string= "file-a"
                       (libgit-pathspec-match-list-entry match-list 0)))

      ;; Find all pathspecs that have no matches
      (should (= 3 (libgit-pathspec-match-list-failed-entrycount match-list)))
      (should (string= "file-b"
                       (libgit-pathspec-match-list-failed-entry match-list 0)))
      (should (string= "file-c"
                       (libgit-pathspec-match-list-failed-entry match-list 1)))
      (should (string= "file-d"
                       (libgit-pathspec-match-list-failed-entry match-list 2))))))
