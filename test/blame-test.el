(ert-deftest blame-file ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let* ((repo (libgit-repository-open path))
           (blame (libgit-blame-file repo "test"))))))

(ert-deftest blame-get-hunk-byindex-byline ()
  (with-temp-dir path
    (init)
    (commit-change "test" "content")
    (let* ((repo (libgit-repository-open path))
           (head (libgit-reference-name-to-id repo "HEAD"))
           (blame (libgit-blame-file repo "test"))
           (hunk-byindex (libgit-blame-get-hunk-byindex blame 0))
           (hunk-byline (libgit-blame-get-hunk-byline blame 1)))
      (dolist (hunk (list hunk-byline hunk-byindex))
        (should (libgit-blame-hunk-p hunk))
        (should (= 1 (libgit-blame-hunk-lines hunk)))
        (should (string= "test" (libgit-blame-hunk-orig-path hunk)))
        (dolist (orig (list nil t))
          (should (string= head (libgit-blame-hunk-commit-id hunk orig)))
          (should (= 1 (libgit-blame-hunk-start-line-number hunk orig)))
          (should (string= "A U Thor" (libgit-signature-name (libgit-blame-hunk-signature hunk orig))))
          (should (string= "author@example.com" (libgit-signature-email (libgit-blame-hunk-signature hunk orig)))))))))

(ert-deftest blame-get-hunk-count ()
  (with-temp-dir path
    (init)
    (commit-change "test" "foo\nbaz")
    (let* ((repo (libgit-repository-open path))
           ;; FIXME: better to rewrite this with HEAD and HEAD^ when
           ;; these abbreviations are supported by libegit.
           (first (libgit-reference-name-to-id repo "HEAD")))
      (commit-change "test" "bar\nbaz")
      (let* ((last  (libgit-reference-name-to-id repo "HEAD"))
             (blame (libgit-blame-file repo "test"))
             (count (libgit-blame-get-hunk-count blame)))
        (should (= 2 count))))))

(ert-deftest blame-file-options-commit-range ()
  (with-temp-dir path
    (init)
    (let ((repo (libgit-repository-open path))
          first second)
      (commit-change "test" "foo\nbaz")
      (setq first (libgit-reference-name-to-id repo "HEAD"))
      (commit-change "test" "bar\nbaz")
      (setq second (libgit-reference-name-to-id repo "HEAD"))

      ;; blame in range [init, first] should return only one hunk
      (let* ((options `((newest-commit . ,first)))
             (blame (libgit-blame-file repo "test" options)))
        (should (= 1 (libgit-blame-get-hunk-count blame))))

      ;; blame in range [second, second] should, again, return only
      ;; one hunk
      (let* ((options `((newest-commit . ,second)
                        (oldest-commit . ,second)))
             (blame (libgit-blame-file repo "test" options)))
        (should (= 1 (libgit-blame-get-hunk-count blame))))

      ;; blame in range [init, second] should return 2 hunks: for the
      ;; first and for the second commit
      (let* ((options `((newest-commit . ,second)))
             (blame (libgit-blame-file repo "test" options)))
        (should (= 2 (libgit-blame-get-hunk-count blame)))))))

(ert-deftest blame-file-options-line-range ()
  (with-temp-dir path
    (init)
    (commit-change "test" "foo\nbar\nbaz")
    (commit-change "test" "foo\nbar\nbum")

    ;; should have 2 hunks: one for "foo" and the other for "baz"
    (let* ((repo (libgit-repository-open path))
           (blame (libgit-blame-file repo "test")))
      (should (= 2 (libgit-blame-get-hunk-count blame))))

    ;; we can achive the same if we specify lines range explicitly
    (let* ((repo (libgit-repository-open path))
           (options '((min-line . 1)
                      (max-line . 3)))
           (blame (libgit-blame-file repo "test" options)))
      (should (= 2 (libgit-blame-get-hunk-count blame))))

    ;; now limit blame to the lines not modified by the second commit:
    ;; we should now see only one (original) hunk
    (let* ((repo (libgit-repository-open path))
           (options '((min-line . 1)
                      (max-line . 2)))
           (blame (libgit-blame-file repo "test" options)))
      (should (= 1 (libgit-blame-get-hunk-count blame))))))

(ert-deftest blame-file-options-first-parent ()
  (with-temp-dir path
    (init)
    (commit-change "test" "foo\nbar\nbaz")
    (create-branch "branch")
    (commit-change "test" "foo\nbum\nbaz")
    (commit-change "test" "foo\nbum\nbin")
    (checkout "master")
    (merge "branch")

    ;; First-parent means that we don't look into 'other' branch when
    ;; a merge commit is reached. Instead, we consider a merge commit
    ;; as an atomic commit.
    ;;
    ;; In this case, instead of 3 distinct hunk, we should see only 2:
    ;; one for the original commit and another for the merge commit.
    (let* ((repo (libgit-repository-open path))
           (options '((first-parent . t)))
           (blame (libgit-blame-file repo "test" options)))
      (should (= 2 (libgit-blame-get-hunk-count blame))))

    ;; in contrast, if we omit the first-parent option, git will check for
    ;; both parents
    (let* ((repo (libgit-repository-open path))
           (blame (libgit-blame-file repo "test")))
      (should (= 3 (libgit-blame-get-hunk-count blame))))))
