;; Use Option as Meta on Mac
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

;; Unbind Mac C-TAB
(global-unset-key [(control tab)])

;; Disable suspend-frame binding
(global-unset-key (kbd "C-z"))

;;;; Keymaps
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

(defvar cjv/window-map (make-sparse-keymap)
  "Keymap for my UI commands.")

(global-set-key (kbd "C-c w") cjv/window-map)

(defvar cjv/toggle-map (make-sparse-keymap)
  "Keymap for toggle commands.")

(global-set-key (kbd "C-c t") cjv/toggle-map)

(defvar cjv/ai-map (make-sparse-keymap)
  "Keymap for AI related commands.")

(global-set-key (kbd "C-c a") cjv/ai-map)

(provide 'init-keybindings)
