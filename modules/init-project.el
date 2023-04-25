(use-package project
  :straight (:type built-in)
  :config
  (defvar cjv/project-ignored-directories '("/opt/homebrew/")
    "Directories ignored by project.el.")

  (defun cjv/project-try-vc-with-ignored-directories (dir)
    "Runs project-try-vc if DIR is not in cjv/project-ignored-directories."
    (when (cl-notany (lambda (ignored-directory)
                         (file-in-directory-p dir ignored-directory))
                       cjv/project-ignored-directories)
      (project-try-vc dir)))

  :custom
  (project-find-functions (list #'cjv/project-try-vc-with-ignored-directories)))

(provide 'init-project)
