(use-package ledger-mode
  :hook (ledger-mode . ledger-flymake-enable)
  :custom
  (ledger-default-date-format "%Y-%m-%d")
  (ledger-post-amount-alignment-column 56)
  (ledger-report-use-strict nil)
  (ledger-reconcile-default-commodity nil))

(defvar cjv/ledger-file (getenv "LEDGER_FILE"))

(defun cjv/ledger-import (file)
  "Import file to ledger"
  (interactive)
  (let* ((cmd (string-join (list "ledger convert"
                                 file
                                 "--input-date-format '%m/%d/%Y'"
                                 "--account 'Assets:Bank:Joint Checking'"
                                 "--no-pager"
                                 "--auto-match"
                                 "--rich-data"
                                 "--date-format '%Y-%m-%d'"
                                 "--invert")
                           " "))
         (result (shell-command-to-string cmd))
         (result (replace-regexp-in-string "\r" "" result))
         (result (replace-regexp-in-string "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\( \\*\\)" "\\1" result)))
    (unless (string= result "")
      (append-to-file "\n" nil cjv/ledger-file)
      (append-to-file result nil cjv/ledger-file))))

;; (cjv/ledger-import "/Users/cjv/Downloads/Chase0125_Activity_20240105.CSV")

(provide 'init-ledger)
