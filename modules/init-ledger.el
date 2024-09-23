(use-package ledger-mode
  :defer t
  :mode ("\\.csv.rules\\'" . ledger-mode)
  :bind (:map ledger-mode-map
              ("C-c C-e" . #'cjv/ledger-toggle-current-transaction-dwim))

  :init
  (defvar cjv/ledger-import-files-directory
    "/Users/cjv/Documents/personal/finance/ledger/import/files")

  (defvar cjv/ledger-import-files-regex
    "\\(Chase[0-9]+_Activity.*\\|Transaction\\|activity\\)\\.csv")

  :config
  (defun cjv/ledger-toggle-transactions-in-region (beg end)
    "Clears all transactions in region from BEG to END."
    (interactive "r")
    (save-excursion
      (save-restriction
        (goto-char beg)
        (beginning-of-line)
        (unless (looking-at ledger-payee-any-status-regex)
          (ledger-navigate-next-xact))
        (while (< (point) end)
          (ledger-toggle-current-transaction)
          (ledger-navigate-next-xact)))))

  (defun cjv/ledger-toggle-current-transaction-dwim ()
    "Toggle the transaction at point or all transactions in region."
    (interactive)
    (if (region-active-p)
        (cjv/ledger-toggle-transactions-in-region (region-beginning) (region-end))
      (ledger-toggle-current-transaction)))

  (defun cjv/ledger-move-import-files ()
    "Moves import files from Downloads to ledger files directory."
    (interactive)
    (dolist (file (directory-files "~/Downloads/" t))
      (message file)
      (let ((filename (file-name-nondirectory file)))
        (when (string-match cjv/ledger-import-files-regex filename)
          (rename-file file (expand-file-name (cjv/append-date-to-filename filename)
                                              cjv/ledger-import-files-directory))))))

  :custom
  (ledger-binary-path "hl")
  (ledger-report-links-in-register nil)
  (ledger-mode-should-check-version nil)
  (ledger-report-auto-width nil)
  (ledger-report-use-native-highlighting nil)
  (ledger-default-date-format "%Y-%m-%d")
  (ledger-post-amount-alignment-column 56)
  (ledger-reports '(("bal" "%(binary) [[ledger-mode-flags]] -f %(ledger-file) bal -V")
                    ("bs" "%(binary) -f %(ledger-file) bs -V")
                    ("checking" "%(binary) -f %(ledger-file) bs 'joint checking' credit")
                    ("is" "%(binary) -f %(ledger-file) is")
                    ("expenses" "%(binary) -f %(ledger-file) is -b '2 months ago' -M -S -2")
                    ("reg" "%(binary) -f %(ledger-file) reg")
                    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                    ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(use-package flymake-hledger
  :ensure (flymake-hledger :fetcher github :repo "DamienCassou/flymake-hledger"
                           :remotes (("fork" :repo "cvdub/flymake-hledger" :protocol ssh)))
  :after ledger-mode
  :hook (ledger-mode . flymake-hledger-enable)
  :custom
  (flymake-hledger-check-ledger-binary-path nil)
  (flymake-hledger-checks '("accounts" "commodities" "ordereddates")))

(provide 'init-ledger)
