(ert-deftest cherrypick ()
  ;; Default
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (create-branch "foo")
    (commit-change "a" "content-a2")
    (checkout "master")

    (let* ((repo (libgit-repository-open path))
	   (cherrypick-commit
	    (libgit-commit-lookup
	     repo
	     (libgit-reference-name-to-id repo "refs/heads/foo"))))
      (should (string= "content-a1" (read-file "a")))
      (libgit-cherrypick repo cherrypick-commit)
      (should (string= "content-a2" (read-file "a")))))

  ;; Merge - 1st parent
  (with-temp-dir path
    (init)
    (commit-change "a" "init")
    (commit-change "b" "init")
    (create-branch "src-1")
    (commit-change "a" "content-src-1")
    (checkout "master")
    (create-branch "src-2")
    (commit-change "b" "content-src-2")
    (merge "src-1")
    (checkout "master")

    (let* ((repo (libgit-repository-open path))
	   (merge-commit
	    (libgit-commit-lookup
	     repo
	     (libgit-reference-name-to-id repo "refs/heads/src-2"))))
      (should (string= "init" (read-file "a")))
      (should (string= "init" (read-file "b")))
      (libgit-cherrypick repo merge-commit nil nil 1)
      (should (string= "content-src-1" (read-file "a")))
      (should (string= "init" (read-file "b")))))

  ;; Merge - 2nd parent
  (with-temp-dir path
    (init)
    (commit-change "a" "init")
    (commit-change "b" "init")
    (create-branch "src-1")
    (commit-change "a" "content-src-1")
    (checkout "master")
    (create-branch "src-2")
    (commit-change "b" "content-src-2")
    (merge "src-1")
    (checkout "master")

    (let* ((repo (libgit-repository-open path))
	   (merge-commit
	    (libgit-commit-lookup
	     repo
	     (libgit-reference-name-to-id repo "refs/heads/src-2"))))
      (should (string= "init" (read-file "a")))
      (should (string= "init" (read-file "b")))
      (libgit-cherrypick repo merge-commit nil nil 2)
      (should (string= "init" (read-file "a")))
      (should (string= "content-src-2" (read-file "b"))))))

(ert-deftest cherrypick-commit ()
  ;; Default
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (create-branch "foo")
    (commit-change "a" "content-a2")
    (checkout "master")

    (let* ((repo (libgit-repository-open path))
	   (cherrypick-commit
	    (libgit-commit-lookup
	     repo
	     (libgit-reference-name-to-id repo "refs/heads/foo")))
	   (our-commit
	    (libgit-commit-lookup
	     repo
	     (libgit-reference-name-to-id repo "HEAD"))))
      ;; libgit-cherrypick-commit should return an index without any
      ;; changes on disk.
      (should (string= "content-a1" (read-file "a")))
      (libgit-cherrypick-commit repo cherrypick-commit our-commit)
      (should (string= "content-a1" (read-file "a")))))

  ;; Merge - 1st parent
  (with-temp-dir path
    (init)
    (commit-change "a" "init")
    (commit-change "b" "init")
    (create-branch "src-1")
    (commit-change "a" "content-src-1")
    (checkout "master")
    (create-branch "src-2")
    (commit-change "b" "content-src-2")
    (merge "src-1")
    (checkout "master")

    (let* ((repo (libgit-repository-open path))
  	   (merge-commit
  	    (libgit-commit-lookup
  	     repo
  	     (libgit-reference-name-to-id repo "refs/heads/src-2")))
  	   (our-commit
  	    (libgit-commit-lookup
  	     repo
  	     (libgit-reference-name-to-id repo "HEAD"))))
      (should (string= "init" (read-file "a")))
      (should (string= "init" (read-file "b")))
      (libgit-cherrypick-commit repo merge-commit our-commit nil 1)
      (should (string= "init" (read-file "a")))
      (should (string= "init" (read-file "b")))))

  ;; Merge - 2nd parent
  (with-temp-dir path
    (init)
    (commit-change "a" "init")
    (commit-change "b" "init")
    (create-branch "src-1")
    (commit-change "a" "content-src-1")
    (checkout "master")
    (create-branch "src-2")
    (commit-change "b" "content-src-2")
    (merge "src-1")
    (checkout "master")

    (let* ((repo (libgit-repository-open path))
  	   (merge-commit
  	    (libgit-commit-lookup
  	     repo
  	     (libgit-reference-name-to-id repo "refs/heads/src-2")))
  	   (our-commit
  	    (libgit-commit-lookup
  	     repo
  	     (libgit-reference-name-to-id repo "HEAD"))))
      (should (string= "init" (read-file "a")))
      (should (string= "init" (read-file "b")))
      (libgit-cherrypick-commit repo merge-commit our-commit nil 2)
      (should (string= "init" (read-file "a")))
      (should (string= "init" (read-file "b"))))))
