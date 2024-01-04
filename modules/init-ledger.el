(use-package ledger-mode
  :custom
  (ledger-binary-path "hl")
  (ledger-report-links-in-register nil)
  (ledger-mode-should-check-version nil)
  (ledger-report-auto-width nil)
  (ledger-report-use-native-highlighting nil)
  (ledger-default-date-format "%Y-%m-%d")
  (ledger-post-amount-alignment-column 56)
  (ledger-reports (("bal" "%(binary) [[ledger-mode-flags]] -f $(ledger-file) bal -V --infer-equity")
                   ("reg" "%(binary) -f %(ledger-file) reg")
                   ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                   ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(use-package flymake-hledger
  :after (ledger-mode flymake)
  :hook (ledger-mode . flymake-hledger-enable)
  :custom
  (flymake-hledger-command ("hl"))
  (flymake-hledger-checks '("accounts" "commodities" "ordereddates")))

(provide 'init-ledger)
