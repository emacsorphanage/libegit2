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
