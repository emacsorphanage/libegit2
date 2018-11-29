(ert-deftest repository-config ()
  (with-temp-dir path
    (init)
    (should (libgit-config-p (libgit-repository-config (libgit-repository-open path))))))

(ert-deftest config-get ()
  (with-temp-dir path
    (init)
    (write ".git/config" "\
[user]
  email = someone@somewhere.com
  name = John Doe
  age = 7
  money = 1k
  hairs = 1m
  atoms = 1g
  male = yes
  intelligent = off
  handsome = 1
  rich = false
")
    (let* ((repo (libgit-repository-open path))
           (config (libgit-repository-config repo))
           (snap (libgit-config-snapshot config)))
      ;; Can't read from a live config object
      (should-error (libgit-config-get-string config "something") :type 'giterr-config)
      (should (string= "someone@somewhere.com" (libgit-config-get-string snap "user.email")))
      (should (string= "John Doe" (libgit-config-get-string snap "user.name")))
      (should (= 7 (libgit-config-get-int snap "user.age")))
      (should (= 1024 (libgit-config-get-int snap "user.money")))
      (should (= (* 1024 1024) (libgit-config-get-int snap "user.hairs")))
      (should (= (* 1024 1024 1024) (libgit-config-get-int snap "user.atoms")))
      (should (libgit-config-get-bool snap "user.male"))
      (should-not (libgit-config-get-bool snap "user.intelligent"))
      (should (libgit-config-get-bool snap "user.handsome"))
      (should-not (libgit-config-get-bool snap "user.rich")))))

(ert-deftest config-set ()
  (with-temp-dir path
    (init)
    (let* ((repo (libgit-repository-open path))
           (config (libgit-repository-config repo))
           trans)

      (libgit-config-set-string config "user.name" "Captain Spiff")
      (let ((snap (libgit-config-snapshot config)))
        (should (string= "Captain Spiff" (libgit-config-get-string snap "user.name")))
        ;; Can't write to a config snapshot
        (should-error (libgit-config-set-string snap "user.name" "Zorg") :type 'giterr-config))

      ;; If the config is locked, writes aren't visible immediately
      (setq trans (libgit-config-lock config))
      (libgit-config-set-string config "user.name" "Wolfgang Amadeus")
      (let ((snap (libgit-config-snapshot config)))
        (should (string= "Captain Spiff" (libgit-config-get-string snap "user.name"))))
      (libgit-transaction-commit trans)
      (let ((snap (libgit-config-snapshot config)))
        (should (string= "Wolfgang Amadeus" (libgit-config-get-string snap "user.name"))))

      (libgit-config-set-int config "user.age" 7)
      (let ((snap (libgit-config-snapshot config)))
        (should (= 7 (libgit-config-get-int snap "user.age"))))

      (libgit-config-set-bool config "user.male" t)
      (let ((snap (libgit-config-snapshot config)))
        (should (libgit-config-get-bool snap "user.male")))

      (libgit-config-set-bool config "user.intelligent" nil)
      (let ((snap (libgit-config-snapshot config)))
        (should-not (libgit-config-get-bool snap "user.intelligent"))))))

(ert-deftest config-levels ()
  (with-temp-dir path
    (write "cfg-a" "\
[user]
  email = someone@somewhere.com
  name = John Doe
  age = 7
  money = 1k
  hairs = 1m
  atoms = 1g
  male = yes
  intelligent = off
  handsome = 1
  rich = false
")
    (write "cfg-b" "\
[user]
  email = someone-else@somewhere-else.com
  name = Jane Doe
  age = 18
  money = 1g
  hairs = 1k
  atoms = 1
  male = no
  intelligent = on
  beautiful = 1
  rich = true
")
    (let ((config (libgit-config-new)))
      (libgit-config-add-file-ondisk config "cfg-a" 'global)
      (libgit-config-add-file-ondisk config "cfg-b" 'local)
      (let ((snap (libgit-config-snapshot config)))
        (should (string= "someone-else@somewhere-else.com" (libgit-config-get-string snap "user.email")))
        (should (string= "Jane Doe" (libgit-config-get-string snap "user.name")))
        (should (= 18 (libgit-config-get-int snap "user.age")))
        (should (= (* 1024 1024 1024) (libgit-config-get-int snap "user.money")))
        (should (= 1024 (libgit-config-get-int snap "user.hairs")))
        (should (= 1 (libgit-config-get-int snap "user.atoms")))
        (should-not (libgit-config-get-bool snap "user.male"))
        (should (libgit-config-get-bool snap "user.handsome"))
        (should (libgit-config-get-bool snap "user.beautiful"))
        (should (libgit-config-get-bool snap "user.rich")))
      (let* ((subconfig (libgit-config-open-level config 'global))
             (snap (libgit-config-snapshot subconfig)))
        (should (string= "someone@somewhere.com" (libgit-config-get-string snap "user.email")))
        (should (string= "John Doe" (libgit-config-get-string snap "user.name")))
        (should (= 7 (libgit-config-get-int snap "user.age")))
        (should (= 1024 (libgit-config-get-int snap "user.money")))
        (should (= (* 1024 1024) (libgit-config-get-int snap "user.hairs")))
        (should (= (* 1024 1024 1024) (libgit-config-get-int snap "user.atoms")))
        (should (libgit-config-get-bool snap "user.male"))
        (should (libgit-config-get-bool snap "user.handsome"))
        (should-error (libgit-config-get-bool snap "user.beautiful") :type 'giterr-config)
        (should-not (libgit-config-get-bool snap "user.rich")))

      ;; Delete only deletes on the highest level
      (libgit-config-delete-entry config "user.rich")
      (let ((snap (libgit-config-snapshot config)))
        (should-not (libgit-config-get-bool snap "user.rich")))

      ;; Deleting again won't work, the lower levels are immutable
      (should-error (libgit-config-delete-entry config "user.rich") :type 'giterr-config))))
