(defun diff-to-data (diff)
  "Helper function to create a lispy data structure from a diff."
  (let (data)
    (libgit-diff-foreach
     diff
     (lambda (delta _)
       (push `(delta
               ,(libgit-diff-delta-file-path delta)
               ,(libgit-diff-delta-file-path delta 'new)
               ,(libgit-diff-delta-nfiles delta)
               ,(libgit-diff-delta-status delta))
             data))
     (lambda (_ _)
       (push 'binary data))
     (lambda (_ hunk)
       (push `(hunk
               ,(libgit-diff-hunk-start hunk)
               ,(libgit-diff-hunk-lines hunk)
               ,(libgit-diff-hunk-start hunk 'new)
               ,(libgit-diff-hunk-lines hunk 'new)
               ,(libgit-diff-hunk-header hunk))
             data))
     (lambda (_ _ line)
       (push `(line
               ,(libgit-diff-line-origin line)
               ,(libgit-diff-line-content line))
             data)))
    (reverse data)))

(ert-deftest diff-one-file-one-hunk ()
  (with-temp-dir path
    (init)
    (commit-change "file" "Line1\nLine2\nLine3\nLine4\n")
    (commit-change "file" "Line5\nLine6\nLine7\nLine8\nLine9\nLine10\n")
    (let* ((repo (libgit-repository-open path))
           (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
           (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
           (diff (libgit-diff-tree-to-tree repo old-tree new-tree))
           (data (diff-to-data diff)))
      (should (equal data
                     '((delta "file" "file" 2 modified)
                       (hunk 1 4 1 6 "@@ -1,4 +1,6 @@\n")
                       (line ?- "Line1\n")
                       (line ?- "Line2\n")
                       (line ?- "Line3\n")
                       (line ?- "Line4\n")
                       (line ?+ "Line5\n")
                       (line ?+ "Line6\n")
                       (line ?+ "Line7\n")
                       (line ?+ "Line8\n")
                       (line ?+ "Line9\n")
                       (line ?+ "Line10\n")))))))

(ert-deftest diff-one-file-one-hunk-no-nl ()
  (with-temp-dir path
    (init)
    (commit-change "file" "Line1\nLine2\nLine3\nLine4")
    (commit-change "file" "Line5\nLine6\nLine7\nLine8\nLine9\nLine10")
    (let* ((repo (libgit-repository-open path))
           (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
           (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
           (diff (libgit-diff-tree-to-tree repo old-tree new-tree))
           (data (diff-to-data diff)))
      (should (equal data
                     '((delta "file" "file" 2 modified)
                       (hunk 1 4 1 6 "@@ -1,4 +1,6 @@\n")
                       (line ?- "Line1\n")
                       (line ?- "Line2\n")
                       (line ?- "Line3\n")
                       (line ?- "Line4")
                       (line ?> "\n\\ No newline at end of file\n")
                       (line ?+ "Line5\n")
                       (line ?+ "Line6\n")
                       (line ?+ "Line7\n")
                       (line ?+ "Line8\n")
                       (line ?+ "Line9\n")
                       (line ?+ "Line10")
                       (line ?< "\n\\ No newline at end of file\n")))))))

(ert-deftest diff-one-file-one-hunk-no-nl-old ()
  (with-temp-dir path
    (init)
    (commit-change "file" "Line1\nLine2\nLine3\nLine4")
    (commit-change "file" "Line5\nLine6\nLine7\nLine8\nLine9\nLine10\n")
    (let* ((repo (libgit-repository-open path))
           (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
           (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
           (diff (libgit-diff-tree-to-tree repo old-tree new-tree))
           (data (diff-to-data diff)))
      (should (equal data
                     '((delta "file" "file" 2 modified)
                       (hunk 1 4 1 6 "@@ -1,4 +1,6 @@\n")
                       (line ?- "Line1\n")
                       (line ?- "Line2\n")
                       (line ?- "Line3\n")
                       (line ?- "Line4")
                       (line ?> "\n\\ No newline at end of file\n")
                       (line ?+ "Line5\n")
                       (line ?+ "Line6\n")
                       (line ?+ "Line7\n")
                       (line ?+ "Line8\n")
                       (line ?+ "Line9\n")
                       (line ?+ "Line10\n")))))))

(ert-deftest diff-one-file-one-hunk-no-nl-new ()
  (with-temp-dir path
    (init)
    (commit-change "file" "Line1\nLine2\nLine3\nLine4\n")
    (commit-change "file" "Line5\nLine6\nLine7\nLine8\nLine9\nLine10")
    (let* ((repo (libgit-repository-open path))
           (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
           (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
           (diff (libgit-diff-tree-to-tree repo old-tree new-tree))
           (data (diff-to-data diff)))
      (should (equal data
                     '((delta "file" "file" 2 modified)
                       (hunk 1 4 1 6 "@@ -1,4 +1,6 @@\n")
                       (line ?- "Line1\n")
                       (line ?- "Line2\n")
                       (line ?- "Line3\n")
                       (line ?- "Line4\n")
                       (line ?+ "Line5\n")
                       (line ?+ "Line6\n")
                       (line ?+ "Line7\n")
                       (line ?+ "Line8\n")
                       (line ?+ "Line9\n")
                       (line ?+ "Line10")
                       (line ?< "\n\\ No newline at end of file\n")))))))

(ert-deftest diff-one-file-two-hunk ()
  (with-temp-dir path
    (init)
    (commit-change "file" "Line1\nLine2\nLine3\nLine4\nLine5\nLine6\nLine7\nLine8\nLine9\nLine10\n")
    (commit-change "file" "Line2\nLine3\nLine4\nLine5\nLine6\nLine7\nLine8\nLine9\n")
    (let* ((repo (libgit-repository-open path))
           (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
           (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
           (diff (libgit-diff-tree-to-tree repo old-tree new-tree))
           (data (diff-to-data diff)))
      (should (equal data
                     '((delta "file" "file" 2 modified)
                       (hunk 1 4 1 3 "@@ -1,4 +1,3 @@\n")
                       (line ?- "Line1\n")
                       (line ?  "Line2\n")
                       (line ?  "Line3\n")
                       (line ?  "Line4\n")
                       (hunk 7 4 6 3 "@@ -7,4 +6,3 @@ Line6\n")
                       (line ?  "Line7\n")
                       (line ?  "Line8\n")
                       (line ?  "Line9\n")
                       (line ?- "Line10\n")))))))

(ert-deftest diff-one-file-two-hunk-context ()
  (with-temp-dir path
    (init)
    (commit-change "file" "Line1\nLine2\nLine3\nLine4\nLine5\nLine6\nLine7\nLine8\nLine9\nLine10\n")
    (commit-change "file" "Line2\nLine3\nLine4\nLine5\nLine6\nLine7\nLine8\nLine9\n")
    (let* ((repo (libgit-repository-open path))
           (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
           (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
           (diff (libgit-diff-tree-to-tree repo old-tree new-tree '((context-lines . 1))))
           (data (diff-to-data diff)))
      (should (equal data
                     '((delta "file" "file" 2 modified)
                       (hunk 1 2 1 1 "@@ -1,2 +1 @@\n")
                       (line ?- "Line1\n")
                       (line ?  "Line2\n")
                       (hunk 9 2 8 1 "@@ -9,2 +8 @@ Line8\n")
                       (line ?  "Line9\n")
                       (line ?- "Line10\n")))))))

(ert-deftest diff-find ()
  (with-temp-dir path
   (init)
   (commit-change "file1" "content")
   (rename-file "file1" "file2")
   (add "file1")
   (add "file2")
   (commit "This should be a rename commit")

   ;; First, ensure that without `libgit-diff-find-similar',
   ;; similarity is not computed and we have a rename operation
   ;; represented by two distinct deltas (one is `added' and another
   ;; is `deleted').
   (let* ((repo (libgit-repository-open path))
          (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
          (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
          (diff (libgit-diff-tree-to-tree repo old-tree new-tree)))
     (should (= 2 (libgit-diff-num-deltas diff)))
     (should (= 1 (libgit-diff-num-deltas diff 'added)))
     (should (= 1 (libgit-diff-num-deltas diff 'deleted))))

   ;; Now run the `libgit-diff-find-similar' and ensure that we have a
   ;; single `renamed' delta.
   (let* ((repo (libgit-repository-open path))
          (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
          (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
          (diff (libgit-diff-tree-to-tree repo old-tree new-tree))
	  (success (libgit-diff-find-similar diff)))
     (should success)
     (should (= 1 (libgit-diff-num-deltas diff)))
     (should (= 1 (libgit-diff-num-deltas diff 'renamed))))

   ;; Try all options and flags
   (let* ((repo (libgit-repository-open path))
          (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
          (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
          (diff (libgit-diff-tree-to-tree repo old-tree new-tree))
          (success (libgit-diff-find-similar
                    diff
                    '((rename-threshold . 20)
                      (rename-from-rewrite-threshold . 30)
                      (copy-threshold . 40)
                      (break-rewrite-threshold . 50)
                      (rename-limit . 60)
                      (metric . nil) ; not implemented, but have to be
                                     ; recognized
                      (flags . (find-renames
                                find-renames-from-rewrites
                                find-copies
                                find-copies-from-unmodified
                                find-rewrites
                                break-rewrites
                                find-for-untracked
                                find-all
                                find-ignore-leading-whitespace
                                find-ignore-whitespace
                                find-dont-ignore-whitespace
                                find-exact-match-only
                                break-rewrites-for-renames-only
                                find-remove-unmodified))))))
     (should success)
     (should (= 1 (libgit-diff-num-deltas diff)))
     (should (= 1 (libgit-diff-num-deltas diff 'renamed))))

   ;; Verify that at least some flags work.
   ;; Make an inexact copy
   (commit-change "file3" "content")
   (delete-file "file3")
   (write "file4" "   content") ;; add a few trailing whitespaces
   (add "file3")
   (add "file4")
   (commit "This should be an inexact rename commit")

   ;; `libgit-diff-find-similar' should not detect a rename if
   ;; `find-dont-ignore-whitespace' flag is set.
   (let* ((repo (libgit-repository-open path))
          (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
          (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
          (diff (libgit-diff-tree-to-tree repo old-tree new-tree))
          (success (libgit-diff-find-similar
                    diff
                    '((flags . (find-renames find-dont-ignore-whitespace))))))
     (should success)
     (should (= 2 (libgit-diff-num-deltas diff)))
     (should (= 0 (libgit-diff-num-deltas diff 'renamed))))

   ;; Now pass an `find-ignore-leading-whitespace' option to detect
   ;; such rename.
   (let* ((repo (libgit-repository-open path))
          (new-tree (libgit-revparse-single repo "HEAD^{tree}"))
          (old-tree (libgit-revparse-single repo "HEAD~1^{tree}"))
          (diff (libgit-diff-tree-to-tree repo old-tree new-tree))
          (success (libgit-diff-find-similar
                    diff
                    '((flags . (find-renames
                                find-ignore-leading-whitespace))))))
     (should success)
     (should (= 1 (libgit-diff-num-deltas diff)))
     (should (= 1 (libgit-diff-num-deltas diff 'renamed))))))
