;;; gpkg.el --- git package management -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Git submodules are terrible and I refuse to use them to manage
;; packages. This package downloads packages directly from Git
;; repositories and compiles them at specific commits.

;; It's a brute-force process with no dependency management. Some
;; packages require massaging in order to get them working, such as
;; using the :removal feature to delete unnecessary files.

;;; Code:

(require 'cl-lib)

(defvar gpkg-root "~/.emacs.d/gpkg"
  "Installation directory for all packages.")

(defvar gpkg-packages ()
  "List of all installed packages.")

(defvar gpkg-removal '("^t$" "^tests?$" "-pkg.el$")
  "Files/directories in packages matching these patterns are deleted.")

(defun gpkg-git (name &rest args)
  "Run git with the given command line arguments."
  (when name
    (push (gpkg-root name) args)
    (push "-C" args))
  (princ (format "%S\n" (cons 'git args)))
  (apply #'call-process "git" nil '(:file "/dev/stdout") nil args))

(cl-defun gpkg-id (name &optional (ref "HEAD"))
  "Return the commit ID for NAME at optional REF (HEAD)."
  (with-temp-buffer
    (call-process "git" nil t nil "-C" (gpkg-root name)
                  "rev-parse" (format "%s^{commit}" ref))
    (buffer-substring (point-min) (1- (point-max)))))

(defun gpkg-purge (dir removal)
  "Recursively delete everything under DIR matching REMOVAL list."
  (dolist (pattern removal)
    (dolist (file (directory-files dir t pattern t))
      (cond ((file-directory-p file)
             (delete-directory file t))
            ((file-exists-p file) (delete-file file)))))
  (dolist (subdir (directory-files dir t "^[^.]" t))
    (when (file-directory-p subdir)
      (gpkg-purge subdir removal))))

(defun gpkg-root (name &rest subdirs)
  "Return the installation root for NAME package."
  (cl-loop with path = (expand-file-name name gpkg-root)
           for dir in subdirs
           do (setf path (expand-file-name dir path))
           finally return path))

(cl-defun gpkg-install (name url ref &key removal)
  "Install package NAME from URL on commit REF."
  (let ((root (gpkg-root name))
        (fresh-clone nil))
    (unless (file-exists-p root)
      (gpkg-git nil "clone" url root)
      (setf fresh-clone t)
      (gpkg-git name "gc"))
    (unless (and (not fresh-clone)
                 (equal (gpkg-id name)
                        (gpkg-id name ref)))
      (gpkg-git name "fetch")
      (gpkg-git name "gc")
      (gpkg-git name "clean" "-dfx")
      (gpkg-git name "reset" "--hard" ref)
      (gpkg-purge root (append removal gpkg-removal)))
    (cl-pushnew name gpkg-packages :test #'equal)
    (cl-pushnew root load-path :test #'equal)))

(defun gpkg-compile ()
  "Byte-compile all installed packages."
  (byte-recompile-directory gpkg-root 0))

(defun gpkg-clean ()
  "Clear out all build files from each package."
  (dolist (package gpkg-packages)
    (gpkg-git package "clean" "-dfx")
    (gpkg-git package "reset" "--hard")))

(defmacro gpkg-config (&rest packages)
  "Thread each list as arguments for `gpkg-install'."
  (cl-loop for package in packages
           collect `(gpkg-install ,@package) into sexp
           finally return `(progn ,@sexp)))

(provide 'gpkg)

;;; gpkg.el ends here
