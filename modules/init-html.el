(use-package web-mode
  :defer t
  :mode ("\\.html?\\'"
         "\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'")
  :init
  ;; Disable pairing {/}
  (add-hook
   'web-mode-hook
   '(lambda ()
      (setq-local electric-pair-inhibit-predicate
                  (lambda (c)
                    (if (char-equal c ?{) t (electric-pair-default-inhibit c)))))))

(provide 'init-html)
