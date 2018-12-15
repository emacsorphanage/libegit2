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

(ert-deftest revert-commit ()
  ;; Default
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (commit-change "a" "content-a2")

    (let* ((repo (libgit-repository-open path))
	   (revert-commit
	    (libgit-commit-lookup
	     repo (libgit-reference-name-to-id repo "HEAD")))
	   (our-commit revert-commit)
	   (index ))
      (should (libgit-revert-commit repo revert-commit our-commit))
      ;; index is in-memory, the file should not change
      (should (string= "content-a2" (read-file "a")))))

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
	     repo (libgit-reference-name-to-id repo "HEAD")))
	   (our-commit revert-commit))
      (should (libgit-revert-commit repo revert-commit our-commit nil 1))
      (should (string= "content-a2" (read-file "a")))))

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
	     repo (libgit-reference-name-to-id repo "HEAD")))
	   (our-commit revert-commit))
      (should (libgit-revert-commit repo revert-commit our-commit nil 2))
      (should (string= "content-a2" (read-file "a"))))))
