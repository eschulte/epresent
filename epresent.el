;;; epresent.el --- Simple presentation mode for Emacs Org-mode

;; Copyright (C) 2008 Tom Tromey <tromey@redhat.com>

;; Author: Tom Tromey <tromey@redhat.com>
;; Created: 12 Jun 2008
;; Version: 0.1
;; Keywords: gui

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a simple presentation mode for Emacs.  It works best in
;; Emacs 23, which has nicer font rendering.

;; To use, invoke epresent-run-frame in an org-mode buffer.  This
;; will make a full-screen frame.  Use n/p to navigate, or q to quit.
;; (There are some other key bindings too.)  Each top-level headline
;; becomes a page in the presentation and Org-mode markup is used
;; to nicely display the buffer's contents.

;;; Code:
(require 'org)

(defface epresent-title-face
  '((t :weight bold :height 1.8 :underline t :inherit variable-pitch))
  "")
(defface epresent-content-face
  '((t :height 1.0 :inherit variable-pitch))
  "")
(defface epresent-list-face
  '((t :height 0.9 :inherit variable-pitch))
  "")
(defface epresent-fixed-face
  '((t :height 1.0 :inherit fixed-pitch))
  "")
(defface epresent-box-face
  '((t :box t :inherit epresent-fixed-face))
  "")
(defface epresent-subtitle-face
  '((t :height 0.7 :inherit variable-pitch))
  "")
(defface epresent-url-face
  '((t :height 1.0 :inherit variable-pitch
       :foreground "blue" :underline t))
  "")
(defface epresent-spacer-face
  '((t :height 0.5 :inherit variable-pitch))
  "")

;; The org buffer for the presentation.
(defvar epresent--org-buffer nil)

;; Our point in the org buffer.
(defvar epresent--org-buffer-point nil)

(defvar epresent--org-top-rx "^[*] ")

(defvar epresent--frame nil
  "Frame for EPresent.")

(defvar epresent-text-scale 3)

(defconst epresent-spacer
  (propertize "\n" 'face 'epresent-spacer-face))

(defun epresent--get-frame ()
  (unless (frame-live-p epresent--frame)
    (setq epresent--frame (make-frame '((minibuffer . nil)
                                        (title . "EPresent")
                                        (fullscreen . fullboth)
                                        (menu-bar-lines . 0)
                                        (tool-bar-lines . 0)
                                        (vertical-scroll-bars . nil)
                                        (left-fringe . 0)
                                        (right-fringe . 0)
                                        (internal-border-width . 20)
                                        (cursor-type . nil)
                                        ))))
  (raise-frame epresent--frame)
  (select-frame-set-input-focus epresent--frame)
  epresent--frame)

;; functions
(defun epresent-display-current-page ()
  "Present the current outline heading."
  (interactive))

(defun epresent-first-page ()
  "Present the first outline heading."
  (interactive))

(defun epresent-display-next-page ()
  "Present the next outline heading."
  (interactive))

(defun epresent-display-previous-page ()
  "Present the previous outline heading."
  (interactive))

(defun epresent-display-quit ()
  "Quit the current presentation."
  (interactive)
  (delete-frame (selected-frame)))

(defun epresent-increase-font ()
  ""
  (interactive)
  (dolist (face
           '(epresent-title-face epresent-content-face epresent-fixed-face))
    (set-face-attribute face nil :height (1+ (face-attribute face :height)))))

(defun epresent-decrease-font ()
  ""
  (interactive)
  (dolist (face
           '(epresent-title-face epresent-content-face epresent-fixed-face))
    (set-face-attribute face nil :height (1- (face-attribute face :height)))))

(defvar epresent-display-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map " " 'epresent-display-next-page)
    (define-key map "n" 'epresent-display-next-page)
    (define-key map [right] 'epresent-display-next-page)
    (define-key map "p" 'epresent-display-previous-page)
    (define-key map [left] 'epresent-display-previous-page)
    (define-key map [backspace] 'epresent-display-previous-page)
    (define-key map "q" 'epresent-display-quit)
    (define-key map "+" 'epresent-increase-font)
    (define-key map "=" 'epresent-increase-font) ; silly binding
    (define-key map "-" 'epresent-decrease-font)
    (define-key map "1" 'epresent-first-page)
    map)
  "Local keymap for EPresent display mode.")

(define-derived-mode epresent-display-mode 'org-mode "EPresent"
  "Lalala."
  (text-scale-adjust 0)
  (text-scale-adjust epresent-text-scale)
  (setq org-inline-image-overlays t)
  (setq org-src-fontify-natively t))

;;;###autoload
(defun epresent-run-frame ()
  (interactive)
  (unless (or (eq major-mode 'outline-mode)
              (eq major-mode 'org-mode))
    (error "EPresent can only be used from Org Mode"))
  (epresent--get-frame)
  (epresent-display-mode)
  (epresent-display-first-page))

;;;###autoload(global-set-key [f12] 'epresent-run-frame)

(provide 'epresent)
;;; epresent.el ends here
