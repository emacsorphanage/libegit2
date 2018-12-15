(ert-deftest revert ()
  ;; Default
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (commit-change "a" "content-a2")

    (let* ((repo (libgit-repository-open path))
	   (revert-commit
	    (libgit-commit-lookup
	     repo (libgit-reference-name-to-id repo "HEAD"))))
      (libgit-revert repo revert-commit)
      (should (string= "content-a1" (read-file "a")))))

  ;; Merge - 1st parent
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (create-branch "foo")
    (commit-change "a" "content-a2")
    (checkout "master")
    (merge "foo")

    (let* ((repo (libgit-repository-open path))
	   (revert-commit
	    (libgit-commit-lookup
	     repo (libgit-reference-name-to-id repo "HEAD"))))
      (libgit-revert repo revert-commit nil nil 1)
      (should (string= "content-a1" (read-file "a")))))

  ;; Merge - 2nd parent
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (create-branch "foo")
    (commit-change "a" "content-a2")
    (checkout "master")
    (merge "foo")

    (let* ((repo (libgit-repository-open path))
	   (revert-commit
	    (libgit-commit-lookup
	     repo (libgit-reference-name-to-id repo "HEAD"))))
      (libgit-revert repo revert-commit nil nil 2)
      (should (string= "content-a2" (read-file "a"))))))
