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

(ert-deftest pathspec-match-tree ()
  (with-temp-dir path
    (init)
    (commit-change "file-a" "content-a")
    (commit-change "child/file-b" "content-b")
    (commit-change "child/file-c" "content-c")

    (let* ((repo (libgit-repository-open path))
           (head (libgit-reference-name-to-id repo "HEAD"))
           (commit (libgit-commit-lookup repo head))
           (root (libgit-commit-tree commit))
	   (child (libgit-tree-lookup
		   repo (caddr (libgit-tree-entry-byname root "child"))))
           (pathspec (libgit-pathspec-new
		      '("file-a" "file-b" "file-c")))
           (match-list-root (libgit-pathspec-match-tree root '(find-failures)
							pathspec))
	   (match-list-child (libgit-pathspec-match-tree child '(find-failures)
							 pathspec)))
      (should match-list-root)
      (should match-list-child)

      ;; Find matched files in root
      (should (= 1 (libgit-pathspec-match-list-entrycount match-list-root)))
      (should (string= "file-a"
                       (libgit-pathspec-match-list-entry match-list-root 0)))

      ;; Find matched files in child
      (should (= 2 (libgit-pathspec-match-list-entrycount match-list-child)))
      (should (string= "file-b"
                       (libgit-pathspec-match-list-entry match-list-child 0)))
      (should (string= "file-c"
                       (libgit-pathspec-match-list-entry match-list-child 1)))

      ;; Find all pathspecs that have no matches
      (should (= 2 (libgit-pathspec-match-list-failed-entrycount match-list-root)))
      (should (string= "file-b"
                       (libgit-pathspec-match-list-failed-entry
			match-list-root 0)))
      (should (string= "file-c"
                       (libgit-pathspec-match-list-failed-entry
			match-list-root 1)))

      (should (= 1 (libgit-pathspec-match-list-failed-entrycount match-list-child)))
      (should (string= "file-a"
                       (libgit-pathspec-match-list-failed-entry
			match-list-child 0))))))

(ert-deftest pathspec-match-diff ()
  (with-temp-dir path
    (init)
    (commit-change "file-a" "content-a")
    (commit-change "file-b" "content-b")
    (commit-change "file-b" "content-b-changed")

    (let* ((repo (libgit-repository-open path))
	   (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
           (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
           (diff (libgit-diff-tree-to-tree repo old-tree new-tree))
           (pathspec (libgit-pathspec-new '("file-a" "file-b")))
           (match-list (libgit-pathspec-match-diff diff '(find-failures)
						   pathspec)))
      (should match-list)

      ;; Find matched files
      (should (= 1 (libgit-pathspec-match-list-entrycount match-list)))
      (should (not (libgit-pathspec-match-list-entry match-list 0)))
      (let ((diff-delta (libgit-pathspec-match-list-diff-entry match-list 0)))
	(should (string= "file-b" (libgit-diff-delta-file-path diff-delta nil)))
	(should (string= "file-b" (libgit-diff-delta-file-path diff-delta t))))

      ;; Find all pathspecs that have no matches
      (should (= 1 (libgit-pathspec-match-list-failed-entrycount match-list)))
      (should (string= "file-a"
                       (libgit-pathspec-match-list-failed-entry
			match-list 0))))))
