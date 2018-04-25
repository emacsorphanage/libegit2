(push (concat default-directory "build") load-path)
(require 'libegit2)

(defmacro with-temp-dir (varnames &rest body)
  (declare (indent 1))
  (let* ((cwd (if (listp varnames) temporary-file-directory varnames))
         (varnames (if (listp varnames) varnames (list varnames)))
         (bindings (mapcar (lambda (sym)
                             (list sym (format "%slibegit2-test-%s/"
                                               temporary-file-directory sym)))
                           varnames)))
    `(let (,@bindings)
       (unwind-protect
           (progn
             ,@(mapcar (lambda (sym) `(make-directory ,sym 'parents)) varnames)
             (let ((default-directory ,cwd))
               ,@body))
         ,@(mapcar (lambda (sym) `(delete-directory ,sym 'recursive)) varnames)))))

(defmacro in-dir (dir &rest body)
  (declare (indent 1))
  `(let ((default-directory ,dir)) ,@body))

(defun run (&rest args)
  (unless (= 0 (apply 'call-process (car args) nil nil nil (cdr args)))
    (error "failed to run '%s'" (mapconcat 'identity args " "))))

(defun write (filename content)
  (-when-let* ((dir (file-name-directory filename)))
    (make-directory dir 'parents))
  (with-temp-file filename
    (insert content)))

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-string)))

(defun read-file-nnl (filename)
  (replace-regexp-in-string "\n*\\'" "" (read-file filename)))

(defun commit (&optional msg)
  (run "git" "commit" "--allow-empty-message" "-m" (or msg "")))

(defun commit-change (filename content &optional msg)
  (write filename content)
  (run "git" "add" filename)
  (commit msg))
