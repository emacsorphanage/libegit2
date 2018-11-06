(ert-deftest repository-config ()
  (with-temp-dir path
    (init)
    (should (libgit-config-p (libgit-repository-config (libgit-repository-open path))))))
