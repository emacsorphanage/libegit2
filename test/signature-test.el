(ert-deftest signature-default ()
  (with-temp-dir path
                 (init)
                 (let ((repo (libgit-repository-open path)))
                   (let ((signature (libgit-signature-default repo)))
                     (should (= signature))))))
