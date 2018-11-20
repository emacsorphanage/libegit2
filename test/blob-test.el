(ert-deftest blob-text ()
  (with-temp-dir path
    (init)
    (commit-change "filename" "line1\nline2\nline3\n")
    (let* ((repo (libgit-repository-open path))
           (blob (libgit-revparse-single repo "HEAD:filename")))
      (should (libgit-blob-p blob))
      (should-not (libgit-blob-binary-p blob))
      (should (= 18 (libgit-blob-rawsize blob)))
      (should (string= "line1\nline2\nline3\n" (libgit-blob-rawcontent blob)))
      (should (string= "line1\nline2\nline3\n" (libgit-blob-filtered-content blob "filename"))))))

(ert-deftest blob-binary ()
  ;; Extracted from an ELF header
  (let* ((str (unibyte-string ?\x7f ?\x45 ?\x4c ?\x46 ?\x02 ?\x01 ?\x01 ?\x00
                              ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00
                              ?\x03 ?\x00 ?\x3e ?\x00 ?\x01 ?\x00 ?\x00 ?\x00
                              ?\xb0 ?\x07 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00
                              ?\x40 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00
                              ?\xb8 ?\x1b ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00
                              ?\x00 ?\x00 ?\x00 ?\x00 ?\x40 ?\x00 ?\x38 ?\x00
                              ?\x09 ?\x00 ?\x40 ?\x00 ?\x1d ?\x00 ?\x1c ?\x00
                              ?\x06 ?\x00 ?\x00 ?\x00 ?\x05 ?\x00 ?\x00 ?\x00
                              ?\x40 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00 ?\x00)))
    (with-temp-dir path
      (init)
      (commit-change "filename" str)
      (let* ((repo (libgit-repository-open path))
             (blob (libgit-revparse-single repo "HEAD:filename")))
        (should (libgit-blob-p blob))
        (should (libgit-blob-binary-p blob))
        (should (= 80 (libgit-blob-rawsize blob)))
        (should (string= str (libgit-blob-rawcontent blob)))
        (should (string= "" (libgit-blob-filtered-content blob "filename")))
        (should (string= str (libgit-blob-filtered-content blob "filename" t)))))))
