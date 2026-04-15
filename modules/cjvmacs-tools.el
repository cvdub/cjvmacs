;;; cjvmacs-tools.el --- Tools config for CJVmacs    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Christian Vanderwall

;; Author: Christian Vanderwall <christian@cvdub.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tools config for CJVmacs

;;; Code:

(use-package eshell
  :defer t
  :commands cjv/open-eshell
  :bind (:map cjv/open-map
              ("e" . #'cjv/open-eshell))
  :hook ((eshell-mode . (lambda ()
                          (remove-hook 'eshell-output-filter-functions 'eshell-postoutput-scroll-to-bottom)))
         (eshell-mode . cjv/eshell-refresh-dir-local-variables)
         (eshell-directory-change . cjv/eshell-refresh-dir-local-variables))
  :config
  (defun cjv/open-eshell (&optional prefix)
    "Opens eshell in a bottom side window."
    (interactive "P")
    (if prefix
        (eshell)
      (cjv/with-bottom-window (eshell))))

  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (defun cjv/eshell-prompt ()
    (let ((cwd (abbreviate-file-name (eshell/pwd)))
          (sym (if (= (user-uid) 0) "# " "$ ")))
      (concat
       ;; line 1: cwd
       cwd
       "\n"
       ;; line 2: prompt symbol
       sym)))

  (setq eshell-prompt-function #'cjv/eshell-prompt
        eshell-prompt-regexp "^[^#$\n]*\n[#$] ")

  (defun cjv/eshell-refresh-dir-local-variables ()
    "Clear and reload dir-local variables."
    (dolist (entry dir-local-variables-alist)
      (kill-local-variable (car entry)))
    (setq file-local-variables-alist nil
          dir-local-variables-alist nil)
    (hack-dir-local-variables-non-file-buffer))

  :custom
  (eshell-visual-commands '("vi" "vim" "nvim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "ncmpcpp"
                            "mutt" "pine" "tin" "trn" "elm"))
  ;; (eshell-visual-subcommands '(("docker" "build")))
  (eshell-banner-message "")
  (eshell-banner-message "")
  (eshell-history-size 100000)
  (eshell-hist-ignoredumps t)
  (eshell-destroy-buffer-when-process-dies t))

(use-package eat
  :defer t
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(use-package tramp
  :defer t)

(use-package transient
  :ensure t)

(use-package magit
  :after (transient)
  :defer t
  :bind (:map magit-section-mode-map
              ("C-<tab>" . nil))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package ansible
  :defer t
  :custom
  (ansible-vault-password-file "/Users/cjv/.config/ansible/vault-password.sh"))

(use-package autothemer
  :defer t)

(use-package yaml-ts-mode
  :defer t
  :mode ("\\.ya?ml\\'")
  :config
  (setq yaml-indent-offset 2))

(use-package pdf-tools
  :defer t
  :init
  (pdf-loader-install))


(use-package systemd
  :defer t)

(use-package sudo-edit
  :defer t)

(use-package jinja2-mode
  :defer t
  :mode ("\\.jinja\\'"))

(use-package dockerfile-mode
  :defer t)

(use-package applescript-mode)

(use-package terraform-mode
  :defer t)

(use-package ledger-mode
  :defer t)

(use-package csv-mode
  :defer t
  :hook (csv-mode . (lambda () (visual-line-fill-column-mode -1))))

(use-package htmlize)

(use-package whisper
  :vc (:url "https://github.com/natrys/whisper.el" :branch "master")
  :bind ("s-`" . #'whisper-run)
  :custom
  (whisper-install-directory no-littering-var-directory)
  (whisper-return-cursor 'end)

  :config
  (setq whisper--ffmpeg-input-device ":5")

  (defun cjv/get-ffmpeg-device ()
    "Gets the list of devices available to ffmpeg.
The output of the ffmpeg command is pretty messy, e.g.
  [AVFoundation indev @ 0x7f867f004580] AVFoundation video devices:
  [AVFoundation indev @ 0x7f867f004580] [0] FaceTime HD Camera (Built-in)
  [AVFoundation indev @ 0x7f867f004580] AVFoundation audio devices:
  [AVFoundation indev @ 0x7f867f004580] [0] Cam Link 4K
  [AVFoundation indev @ 0x7f867f004580] [1] MacBook Pro Microphone
so we need to parse it to get the list of devices.
The return value contains two lists, one for video devices and one for audio devices.
Each list contains a list of cons cells, where the car is the device number and the cdr is the device name."
    (unless (string-equal system-type "darwin")
      (error "This function is currently only supported on macOS"))

    (let ((lines (string-split (shell-command-to-string "ffmpeg -list_devices true -f avfoundation -i dummy || true") "\n")))
      (cl-loop with at-video-devices = nil
               with at-audio-devices = nil
               with video-devices = nil
               with audio-devices = nil
               for line in lines
               when (string-match "AVFoundation video devices:" line)
               do (setq at-video-devices t
                        at-audio-devices nil)
               when (string-match "AVFoundation audio devices:" line)
               do (setq at-audio-devices t
                        at-video-devices nil)
               when (and at-video-devices
                         (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
               do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) video-devices)
               when (and at-audio-devices
                         (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
               do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) audio-devices)
               finally return (list (nreverse video-devices) (nreverse audio-devices)))))

  (defun cjv/find-device-matching (string type)
    "Get the devices from `cjv/get-ffmpeg-device' and look for a device
matching `STRING'. `TYPE' can be :video or :audio."
    (let* ((devices (cjv/get-ffmpeg-device))
           (device-list (if (eq type :video)
                            (car devices)
                          (cadr devices))))
      (cl-loop for device in device-list
               when (string-match-p string (cdr device))
               return (car device))))

  (defcustom cjv/default-audio-device nil
    "The default audio device to use for whisper.el and outher audio processes."
    :type 'string)

  (defun cjv/select-default-audio-device (&optional device-name)
    "Interactively select an audio device to use for whisper.el and other audio processes.
If `DEVICE-NAME' is provided, it will be used instead of prompting the user."
    (interactive)
    (let* ((audio-devices (cadr (cjv/get-ffmpeg-device)))
           (indexes (mapcar #'car audio-devices))
           (names (mapcar #'cdr audio-devices))
           (name (or device-name (completing-read "Select audio device: " names nil t))))
      (setq cjv/default-audio-device (cjv/find-device-matching name :audio))
      (when (boundp 'whisper--ffmpeg-input-device)
        (setq whisper--ffmpeg-input-device (format ":%s" cjv/default-audio-device))))))



(provide 'cjvmacs-tools)

;;; cjvmacs-tools.el ends here
