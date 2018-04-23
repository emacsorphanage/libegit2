(push (concat default-directory "build") load-path)
(require 'libegit2)

(defmacro with-temp-dir (varname &rest body)
  (declare (indent 1))
  (let ((path (concat temporary-file-directory "libegit2-test/")))
    `(unwind-protect
         (progn
           (make-directory ,path)
           (let ((,varname ,path) (default-directory ,path))
             ,@body))
       (delete-directory ,path 'recursive))))

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
