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

(provide 'init-keybindings)
