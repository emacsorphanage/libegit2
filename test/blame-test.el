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
           (hunk-byindex  (libgit-blame-get-hunk-byindex blame 0))
           (hunk-byline   (libgit-blame-get-hunk-byline blame 1))
           (expected-hunk `((lines-in-hunk . 1)
                            (final-commit-id . ,head)
                            (final-start-line-number . 1)
                            (orig-commit-id . ,head)
                            (orig-path . "test")
                            (orig-start-line-number . 1)
                            (boundary . t))))
      (should (equal expected-hunk hunk-byindex))
      (should (equal expected-hunk hunk-byline)))))

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
