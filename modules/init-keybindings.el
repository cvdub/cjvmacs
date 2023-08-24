(defvar cjv/global-keymap (make-keymap)
  "Keymap for cjv/keybindings-mode.")

(define-minor-mode cjv/keybindings-mode
  "Minor mode for custom keybindings."
  :init-value t
  :global t
  :keymap cjv/global-keymap)

;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((cjv/keybindings-mode . ,cjv/global-keymap)))
(define-key cjv/global-keymap (kbd "M-o") #'other-window)


;; Disable suspend-frame binding
(global-unset-key (kbd "C-z"))

(defvar cjv/open-map (make-sparse-keymap)
  "Keymap for opening stuff.")

(global-set-key (kbd "C-c o") cjv/open-map)

(defvar cjv/code-map (make-sparse-keymap)
  "Keymap for code related commands.")

(global-set-key (kbd "C-c c") cjv/code-map)

(defvar cjv/my-map (make-sparse-keymap)
  "Keymap for my personal commands.")

(global-set-key (kbd "C-c m") cjv/my-map)

(defvar cjv/notes-map (make-sparse-keymap)
  "Keymap for notes related commands.")

(global-set-key (kbd "C-c n") cjv/notes-map)

(defun cjv/open-downloads ()
  "Opens the downloads directory."
  (interactive)
  (find-file "~/Downloads/"))

(global-set-key (kbd "<f6>") #'cjv/open-downloads)

(defun cjv/open-desktop ()
  "Opens the desktop directory."
  (interactive)
  (find-file "~/Desktop/"))

(global-set-key (kbd "<S-f6>") #'cjv/open-desktop)

(provide 'init-keybindings)
