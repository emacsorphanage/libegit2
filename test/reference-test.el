(ert-deftest reference-create ()
  (with-temp-dir path
    (run "git" "init")
    (commit-change "test" "contents")
    (let ((repo (libgit-repository-open path))
          (id (read-file-nnl ".git/refs/heads/master")))
      (libgit-reference-create repo "OMFG" id)
      (should (string= id (read-file-nnl ".git/OMFG")))
      (libgit-reference-create repo "refs/something" id)
      (should (string= id (read-file-nnl ".git/refs/something")))
      (should-error (libgit-reference-create repo "OMFG" id) :type 'giterr)
      (libgit-reference-create repo "OMFG" id 'force)
      (should (string= id (read-file-nnl ".git/OMFG"))))))

(ert-deftest reference-create-matching ()
  (with-temp-dir path
    (run "git" "init")
    (commit-change "test" "contents")
    (let ((repo (libgit-repository-open path))
          (current-id (read-file-nnl ".git/refs/heads/master")))
      (libgit-reference-create-matching repo "OMFG" current-id)
      (commit-change "test" "contents2")
      (let ((new-id (read-file-nnl ".git/refs/heads/master")))
        (should-error (libgit-reference-create-matching repo "OMFG" new-id) :type 'giterr)
        (libgit-reference-create-matching repo "OMFG" new-id 'force)
        (should (string= new-id (read-file-nnl ".git/OMFG")))
        (libgit-reference-create-matching repo "OMFG" current-id 'force new-id)
        (should (string= current-id (read-file-nnl ".git/OMFG")))
        (should-error (libgit-reference-create-matching
                       repo "OMFG" new-id 'force (substring current-id 1))
                      :type 'giterr)))))

(ert-deftest reference-dup ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-dwim ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-lookup ()
  (with-temp-dir path
    (run "git" "init")
    (commit-change "test" "contents")
    (let* ((repo (libgit-repository-open path)))
      (libgit-reference-lookup repo "refs/heads/master")
      (libgit-reference-lookup repo "HEAD")
      (should-error (libgit-reference-lookup repo "nonexistent" :type 'giterr)))))

(ert-deftest reference-list ()
  (with-temp-dir path
    (run "git" "init")
    (commit-change "test" "contents")
    (let* ((repo (libgit-repository-open path))
           (id (read-file-nnl ".git/refs/heads/master")))
      (libgit-reference-create repo "OMFG" id)
      (libgit-reference-create repo "refs/something" id)
      (let ((refs (libgit-reference-list repo)))
        (should (member "refs/heads/master" refs))
        (should (member "refs/something" refs))
        (should (= 2 (length refs)))))))

(ert-deftest reference-name ()
  (with-temp-dir path
    (run "git" "init")
    (commit-change "test" "contents")
    (let ((repo (libgit-repository-open path)))
      (should (string= "HEAD" (libgit-reference-name (libgit-reference-lookup repo "HEAD"))))
      (should (string= "refs/heads/master"
                       (libgit-reference-name (libgit-reference-lookup repo "refs/heads/master")))))))

(ert-deftest reference-name-to-id ()
  (with-temp-dir path
    (run "git" "init")
    (commit-change "test" "contents")
    (let* ((repo (libgit-repository-open path)))
      (should (string= (read-file-nnl ".git/refs/heads/master")
                       (libgit-reference-name-to-id repo "refs/heads/master")))
      (should (string= (read-file-nnl ".git/refs/heads/master")
                       (libgit-reference-name-to-id repo "HEAD"))))))

(ert-deftest reference-owner ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-peel ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-resolve ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-shorthand ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-symbolic-target ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-target ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-target-peel ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-target-type ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-delete ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-ensure-log ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-remove ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-branch-p ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-direct-p ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-has-log-p ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-note-p ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-remote-p ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-symbolic-p ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-tag-p ()
  ;; TODO
  (skip-unless nil))

(ert-deftest reference-valid-name-p ()
  ;; TODO
  (skip-unless nil))
