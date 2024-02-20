(use-package project
  :ensure nil
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
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             (magit-project-status "Magit" ?m)
                             (project-eshell "Eshell")))
  (project-find-functions (list #'cjv/project-try-vc-with-ignored-directories)))

(use-package projectile
  :disabled)

(provide 'init-project)
