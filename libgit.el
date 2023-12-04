;;; libgit.el --- Thin bindings to libgit2. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Eivind Fonn
;; Copyright (C) 2018-2023 The Magit Project Contributors

;; Author: Eivind Fonn <evfonn@gmail.com>
;; Homepage: https://github.com/magit/libegit2
;; Keywords: git vc

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; SPDX-License-Identifier: GPL-2.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides thin bindings to libgit2. To use these bindings,
;; issue a call to (require 'libgit). This will load the dynamic module,
;; or prompt the user to build it.

;;; Code:

(unless module-file-suffix
  (error "Module support not detected, libgit can't work"))

(defgroup libgit nil
  "Customizations for libgit."
  :group 'magit)

(defcustom libgit-auto-rebuild nil
  "Whether libgit should be rebuilt noninteractively when it cannot be loaded."
  :group 'libgit
  :type 'boolean)

(defvar libgit--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where libgit is installed.")

(defvar libgit--build-dir
  (expand-file-name "build" libgit--root)
  "Directory where the libegit2 dynamic module file should be built.")

(defvar libgit--module-file-name
  (file-name-with-extension "libegit2" module-file-suffix)
  "Name of the libegit2 dynamic module file.")

(defvar libgit--module-file-name-path
  (expand-file-name libgit--module-file-name
                    libgit--build-dir)
  "Path to the libegit2 dynamic module file.")

(defun libgit--configure ()
  "Run the configure step of libegit2 asynchronously.

On successful exit, pass control on to the build step."
  (make-directory libgit--build-dir 'parents)
  (let ((default-directory libgit--build-dir))
    (set-process-sentinel
     (start-process "libgit-cmake" "*libgit build*" "cmake" "..")
     (lambda (proc _event)
       (when (eq 'exit (process-status proc))
         (if (= 0 (process-exit-status proc))
             (libgit--build)
           (pop-to-buffer "*libgit build*")
           (error "libgit: configuring failed with exit code %d" (process-exit-status proc))))))))

(defun libgit--build ()
  "Run the build step of libegit2 asynchronously.

On successful exit, pass control on to the load step."
  (let ((default-directory libgit--build-dir))
    (set-process-sentinel
     (start-process "libgit-cmake" "*libgit build*" "make")
     (lambda (proc _event)
       (when (eq 'exit (process-status proc))
         (if (= 0 (process-exit-status proc))
             (libgit--load)
           (pop-to-buffer "*libgit build*")
           (error "libgit: building failed with exit code %d" (process-exit-status proc))))))))

(defun libgit--load ()
  "Load the `libegit2' dynamic module.
If that fails, then raise an error."
  (unless (featurep 'libegit2)
    (load libgit--module-file-name-path nil t t))
  (unless (featurep 'libegit2)
    (error "libgit: unable to load the libegit2 dynamic module")))

;;;###autoload
(defun libgit-load ()
  "Load the `libegit2' dynamic module.
If the module is not available, then offer to build it."
  (interactive)
  (cond
    ((file-exists-p libgit--module-file-name-path)
     (libgit--load))
    ((locate-library libgit--module-file-name t)
     (setq libgit--module-file-name-path (locate-library libgit--module-file-name t))
     (libgit--load))
   (libgit-auto-rebuild
    (libgit--configure))
   ((and (not noninteractive)
         (y-or-n-p "libgit must be built, do so now?"))
    (libgit--configure))
   (noninteractive
    (message "libgit was not loaded!"))
   (t
    (error "libgit was not loaded!"))))

(libgit-load)

(provide 'libgit)

;;; libgit.el ends here
