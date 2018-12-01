(ert-deftest reset ()
  ;; Soft reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit-repository-open path))
	   (reset-to
	    (libgit-object-lookup
	     repo (libgit-reference-name-to-id repo "HEAD"))))

      (commit-change "a" "content-a2")
      (libgit-reset repo reset-to 'soft)

      ;; Head should reset, content of a file is preserved, file is
      ;; staged.
      (should (string= (libgit-reference-name-to-id repo "HEAD")
		       (libgit-object-id reset-to)))
      (should (string= (read-file "a") "content-a2"))
      (should (member 'index-modified (libgit-status-file repo "a")))))

  ;; Mixed reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit-repository-open path))
  	   (reset-to
  	    (libgit-object-lookup
  	     repo (libgit-reference-name-to-id repo "HEAD"))))

      (commit-change "a" "content-a2")
      (libgit-reset repo reset-to 'mixed)

      ;; Head should reset, content of a file is preserved, file is
      ;; unstaged.
      (should (string= (libgit-reference-name-to-id repo "HEAD")
  		       (libgit-object-id reset-to)))
      (should (string= (read-file "a") "content-a2"))
      (should (member 'wt-modified (libgit-status-file repo "a")))))

  ;; Hard reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit-repository-open path))
  	   (reset-to
  	    (libgit-object-lookup
  	     repo (libgit-reference-name-to-id repo "HEAD"))))

      (commit-change "a" "content-a2")
      (libgit-reset repo reset-to 'hard)

      ;; Head should reset, content of a file is lost.
      (should (string= (libgit-reference-name-to-id repo "HEAD")
  		       (libgit-object-id reset-to)))
      (should (string= (read-file "a") "content-a1"))
      (should (eq nil (libgit-status-file repo "a"))))))

(ert-deftest reset-from-annotated ()
  ;; Soft reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit-repository-open path))
	   (reset-to
	    (libgit-annotated-commit-from-revspec repo "HEAD")))

      (commit-change "a" "content-a2")
      (libgit-reset-from-annotated repo reset-to 'soft)

      ;; Head should reset, content of a file is preserved, file is
      ;; staged.
      (should (string= (libgit-reference-name-to-id repo "HEAD")
		       (libgit-annotated-commit-id reset-to)))
      (should (string= (read-file "a") "content-a2"))
      (should (member 'index-modified (libgit-status-file repo "a")))))

  ;; Mixed reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit-repository-open path))
  	   (reset-to
  	    (libgit-annotated-commit-from-revspec repo "HEAD")))

      (commit-change "a" "content-a2")
      (libgit-reset-from-annotated repo reset-to 'mixed)

      ;; Head should reset, content of a file is preserved, file is
      ;; unstaged.
      (should (string= (libgit-reference-name-to-id repo "HEAD")
  		       (libgit-annotated-commit-id reset-to)))
      (should (string= (read-file "a") "content-a2"))
      (should (member 'wt-modified (libgit-status-file repo "a")))))

  ;; Hard reset
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (let* ((repo (libgit-repository-open path))
  	   (reset-to
  	    (libgit-annotated-commit-from-revspec repo "HEAD")))

      (commit-change "a" "content-a2")
      (libgit-reset-from-annotated repo reset-to 'hard)

      ;; Head should reset, content of a file is lost.
      (should (string= (libgit-reference-name-to-id repo "HEAD")
  		       (libgit-annotated-commit-id reset-to)))
      (should (string= (read-file "a") "content-a1"))
      (should (eq nil (libgit-status-file repo "a"))))))

(ert-deftest reset-default ()
  (with-temp-dir path
    (init)
    (commit-change "a" "content-a1")
    (commit-change "b" "content-b1")
    (let* ((repo (libgit-repository-open path))
	   (reset-to
	    (libgit-object-lookup
	     repo (libgit-reference-name-to-id repo "HEAD"))))

      (write "a" "content-a2")
      (write "b" "content-b2")
      (add "a" "b")
      (libgit-reset-default repo reset-to '("a"))

      ;; Head should not move, files matching pathspec should become
      ;; unstaged.
      (should (string= (libgit-reference-name-to-id repo "HEAD")
		       (libgit-object-id reset-to)))
      (should (string= (read-file "a") "content-a2"))
      (should (string= (read-file "b") "content-b2"))
      (should (member 'wt-modified (libgit-status-file repo "a")))
      (should (member 'index-modified (libgit-status-file repo "b"))))))
