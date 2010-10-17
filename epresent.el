;;; epresent.el --- Simple presentation mode for Emacs

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

;; To use, invoke epresent-run-frame in an outline-mode buffer.  This
;; will make a full-screen frame.  Use n/p to navigate, or q to quit.
;; (There are some other key bindings too.)  Some special markup is
;; available as well:

;;   "#" marks a comment line
;;   "* @Something" means that this outline node starts a new page
;;   "@toc" by itself on a line means to insert a table-of-contents here
;;   "[File Text]" means insert FILE as an image, with TEXT underneath
;;   "=Text=" means to format text using a mono font, useful for code

;; Probably this should be redone as a muse back end, using muse
;; markup instead.


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

;; The outline buffer for the presentation.
(defvar epresent--outline-buffer nil)

;; Our point in the outline buffer.
(defvar epresent--outline-buffer-point nil)

(defvar epresent--outline-top-rx "^[*] ")

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

(defun epresent-erase-buffer (buffer)
  (with-current-buffer buffer
    (erase-buffer)))

(defun epresent-insert (buffer string)
  (with-current-buffer buffer
    (insert string epresent-spacer)))

(defun epresent-insert-2nd (buffer string)
  (with-current-buffer buffer
    (insert "  " (propertize string 'face 'epresent-content-face)
            epresent-spacer)))

(defun epresent-insert-list-element (buffer string)
  (with-current-buffer buffer
    (insert "  " (propertize (concat "• " string) 'face 'epresent-list-face)
            epresent-spacer)))

(defun epresent-insert-code (buffer string)
  (let (in-box
        (result "    "))
    (dolist (text (split-string string "!"))
      (setq result (concat result
                           (propertize text
                                       'face (if in-box 'epresent-box-face
                                               'epresent-fixed-face))))
      (setq in-box (not in-box)))
    (epresent-insert buffer (concat result epresent-spacer))))

(defun epresent-insert-font-lock (buffer mode string)
  (epresent-insert buffer
                   (with-temp-buffer
                     (funcall (intern mode))
                     (insert string)
                     (indent-region 0 (point-max))
                     (goto-char (point-max))
                     (beginning-of-line)
                     (open-rectangle 0 (+ 4 (point)))
                     (font-lock-fontify-buffer)
                     (buffer-string))))

(defun epresent-insert-url (buffer string)
  (with-current-buffer buffer
    (insert "  " (propertize string 'face 'epresent-url-face)
            epresent-spacer)))

(defun epresent-insert-image (buffer file text)
  (let ((full-name (expand-file-name file)))
    (with-current-buffer buffer
      ;; TODO: a way to flush the image cache
      (let ((image (create-image full-name)))
        (insert-image image)
        (insert "\n")
        (when text
          (insert (propertize " " 'display `(space :align-to (0.7 . ,image))))
          (insert (propertize text 'face 'epresent-subtitle-face) epresent-spacer))))))

(defun epresent-display-render-page (buffer)
  (let (seen done)
    (while (not done)
      (cond
       ((eobp)
        (setq done t))
       ((looking-at "^#")       ; Comment
        nil)
       ((looking-at "^[*] @?\\(.*\\)$") ; New page.
        (if seen
            (progn
              (setq done t)
              (forward-line -1))
          (setq seen t)
          (epresent-erase-buffer buffer)
          (epresent-insert buffer "\n")
          (epresent-insert buffer (propertize (match-string 1)
                                              'face 'epresent-title-face))
          (epresent-insert buffer "\n")))
       ((looking-at "^$")       ; Blank line.
        (epresent-insert buffer epresent-spacer))
       ;; TODO: URLs should be font-locked anywhere, not just headers
       ((looking-at "^[*][*] \\(http.*\\)") ; URLs
        (epresent-insert-url buffer (match-string 1)))
       ((looking-at "^[*][*] \\(.*\\)$") ; Sub-heading.
        (epresent-insert-2nd buffer (match-string 1)))
       ((looking-at "^[*][*][*] \\(.*\\)$") ; List
        (epresent-insert-list-element buffer (match-string 1)))
       ((looking-at "^@toc$")       ; Table of contents.
        (save-excursion
          (while (re-search-forward "^[*] @\\(.*\\)$" nil t)
            (epresent-insert-2nd buffer (match-string 1)))))
       ((looking-at "^\\[\\([^ ]*\\) ?\\(.*\\)\\]$") ; Image: [file text]
        (epresent-insert-image buffer (match-string 1) (match-string 2)))
       ;; Font-locked code: -|mode-name\n [...]|
       ((looking-at " *-|\\([-[:alnum:]]+\\)\n\\(\\(.\\|\n\\)+?\\)|")
        (epresent-insert-font-lock buffer (match-string 1) (match-string 2)))
       ((looking-at "^=\\(.*\\)=$") ; Code.
        (epresent-insert-code buffer (match-string 1))))
      (forward-line))))

(defun epresent-display-next-page ()
  (interactive)
  (let ((buffer (current-buffer))
        (pt epresent--outline-buffer-point))
    (save-excursion
      (set-buffer epresent--outline-buffer)
      (goto-char pt)
      (forward-line)
      (when (re-search-forward epresent--outline-top-rx nil t)
        (beginning-of-line)
        (setq epresent--outline-buffer-point (point))
        (epresent-display-render-page buffer)))))

(defun epresent-display-previous-page ()
  (interactive)
  (let ((buffer (current-buffer))
        (pt epresent--outline-buffer-point))
    (save-excursion
      (set-buffer epresent--outline-buffer)
      (goto-char pt)
      (when (re-search-backward epresent--outline-top-rx nil t)
        (setq epresent--outline-buffer-point (point))
        (epresent-display-render-page buffer)))))

(defun epresent-first-page ()
  (interactive)
  (setq epresent--outline-buffer-point
        (with-current-buffer epresent--outline-buffer (point-min)))
  (epresent-display-next-page))

(defun epresent-display-quit ()
  (interactive)
  (delete-frame (selected-frame)))

(defun epresent-increase-font ()
  (interactive)
  (dolist (face
           '(epresent-title-face epresent-content-face epresent-fixed-face))
    (set-face-attribute face nil :height (1+ (face-attribute face :height)))))

(defun epresent-decrease-font ()
  (interactive)
  (dolist (face
           '(epresent-title-face epresent-content-face epresent-fixed-face))
    (set-face-attribute face nil :height (1- (face-attribute face :height)))))

(defun epresent-debug-visit-otl ()
  (interactive)
  (switch-to-buffer-other-window epresent--outline-buffer)
  (goto-char epresent--outline-buffer-point))

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
    (define-key map "$" 'epresent-debug-visit-otl)
    (define-key map "1" 'epresent-first-page)
    map)
  "Local keymap for EPresent display mode.")

(define-derived-mode epresent-display-mode nil "EPresent"
  "Lalala."
  ;; fill-column setting?  or somehow use :align-to?
  ;; These have to be global for now.  FIXME.
  ;; (make-local-variable 'epresent--outline-buffer)
  ;; (make-local-variable 'epresent--outline-buffer-point)
  (set (make-local-variable 'mode-line-format) nil)
  (text-scale-adjust 0)
  (text-scale-adjust epresent-text-scale))

;;;###autoload
(defun epresent-run-frame ()
  (interactive)
  (unless (or (eq major-mode 'outline-mode)
              (eq major-mode 'org-mode))
    (error "EPresent can only be used from Outline Mode"))
  (let ((out-buf (current-buffer))
        (out-point (point-min))
        (buffer (get-buffer-create "*EPresent*")))
    (epresent--get-frame)
    (switch-to-buffer buffer)
    (epresent-display-mode)
    (setq epresent--outline-buffer out-buf)
    (setq epresent--outline-buffer-point out-point)
    (epresent-display-next-page)))

;;;###autoload(global-set-key [f12] 'epresent-run-frame)

(provide 'epresent)
;;; epresent.el ends here
