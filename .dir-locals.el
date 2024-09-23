;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((eval . (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc t)))))
