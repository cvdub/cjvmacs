;;; wesley-theme.el --- The perfect theme -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Christian Vanderwall

;; Author: Christian Vanderwall <christian@cvdub.net>
;; Keywords: faces

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

;; The perfect theme. Uses autothemer.

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(add-to-list 'custom-theme-load-path "/Users/cjv/.config/emacs/themes/")

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(defun reload-wesley ()
  (interactive)
  (message "Reloading Wesley")
  (load-theme 'wesley t))

(bind-key (kbd "t") #'reload-wesley 'cjv/toggle-map)

(defun disable-wesley ()
  (interactive)
  (message "Disabling Wesley")
  (disable-theme 'wesley))

(bind-key (kbd "T") #'disable-wesley 'cjv/toggle-map)

(autothemer-deftheme
 wesley "The perfect theme"
 ;; Specify the color classes used by the theme
 ((((class color) (min-colors #xFFFFFF)))

  ;; Specify the color palette, color columns correspond to each of the classes above.
  (bg-0 "#021112")
  (bg-1 "#041E20")
  (bg-2 "#073336")
  (bg-3 "#094349")
  (bg-4 "#0B535B")
  (bg-5 "#0D636D")

  (fg-0 "#F7F6F3")
  (fg-1 "#F4F3EE")
  (fg-2 "#EFEDE6")
  (fg-3 "#E7E4DA")
  (fg-4 "#D7D2C1")
  (fg-5 "#CFC9B4")

  (dark-green "#446C37")
  (green "#66A253")
  (light-green "#94C186")

  (dark-blue "#4E81BC")
  (blue "#6290C3")
  (light-blue "#89ABD2")

  (dark-red "#BC4E4E")
  (red "#C36060")
  (light-red "#D28989")

  (dark-magenta "#A3008D")
  (magenta "#C900AF")
  (light-magenta "#F500D4")

  (dark-orange "#EC6A32")
  (orange "#EF8354")
  (light-orange "#F3A07C")

  )

 ;; Specifications for Emacs faces.
 ;; Simpler than deftheme, just specify a face name and
 ;; a plist of face definitions (nested for :underline, :box etc.)
 ((default (:background bg-1 :foreground fg-1))
  (cursor (:background fg-1))
  (mode-line (:background bg-3 :foreground fg-2 :box nil))
  (mode-line-inactive (:background bg-1 :foreground fg-4 :box nil))
  (fringe (:background bg-1))
  (hl-line (:background bg-2))
  (region (:background bg-3))
  (secondary-selection (:background bg-2))
  (minibuffer-prompt (:foreground light-blue :bold t))
  (vertical-border (:foreground bg-2))
  (internal-border (:foreground bg-2))
  (link (:foreground light-blue :underline t))
  (shadow (:foreground bg-4))

  (font-lock-builtin-face (:foreground light-blue))
  (font-lock-comment-face (:foreground bg-5))
  (font-lock-constant-face (:foreground light-orange))
  (font-lock-function-name-face (:foreground light-blue))
  (font-lock-keyword-face (:foreground blue))
  (font-lock-string-face (:foreground fg-4))
  (font-lock-number-face (:foreground fg-4))
  (font-lock-variable-name-face (:foreground light-blue))
  (font-lock-type-face (:foreground light-orange))
  (font-lock-property-face (:foreground light-blue))
  (font-lock-warning-face (:foreground light-red :bold t))

  (error (:foreground red :bold t))
  (success (:foreground light-green :bold t))
  (warning (:foreground orange :bold t))
  (alert-low-face (:foreground light-blue))
  (trailing-whitespace (:background dark-red))
  (escape-glyph (:foreground light-blue))
  (header-line (:background bg-2 :foreground fg-2 :box nil :inherit nil))
  (highlight (:background bg-4 :foreground fg-1))
  (homoglyph (:foreground light-orange))
  (match (:foreground bg-1 :background light-blue))
  )


 ;; Forms after the face specifications are evaluated.
 ;; (palette vars can be used, read below for details.)

 ;; (custom-theme-set-variables 'example-name
 ;;                             `(ansi-color-names-vector [,example-red
 ;;                                                        ,example-green
 ;;                                                        ,example-blue
 ;;                                                        ,example-purple
 ;;                                                        ,example-yellow
 ;;                                                        ,example-orange
 ;;                                                        ,example-cyan]))
 )

(provide-theme 'wesley)

;;; wesley-theme.el ends here
