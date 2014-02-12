;;; epresent.el --- Simple presentation mode for Emacs Org-mode

;; Copyright (C) 2008 Tom Tromey <tromey@redhat.com>
;;               2010 Eric Schulte <schulte.eric@gmail.com>

;; Authors: Tom Tromey <tromey@redhat.com>, Eric Schulte <schulte.eric@gmail.com>
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

;; To use, invoke epresent-run in an org-mode buffer.  This
;; will make a full-screen frame.  Use n/p to navigate, or q to quit.
;; (There are some other key bindings too.)  Each top-level headline
;; becomes a page in the presentation and Org-mode markup is used
;; to nicely display the buffer's contents.

;;; Code:
(require 'org)
(require 'ox)
(require 'ox-latex)
(require 'cl-lib)

(defface epresent-title-face
  '((t :weight bold :height 360 :underline t :inherit variable-pitch))
  "")
(defface epresent-heading-face
  '((t :weight bold :height 270 :underline t :inherit variable-pitch))
  "")
(defface epresent-subheading-face
  '((t :weight bold :height 240 :inherit variable-pitch))
  "")
(defface epresent-author-face
  '((t :height 1.6 :inherit variable-pitch))
  "")
(defface epresent-bullet-face
  '((t :weight bold :height 1.4 :underline nil :inherit variable-pitch))
  "")
(defface epresent-hidden-face
  '((t :invisible t))
  "")

(defvar epresent--frame nil
  "Frame for EPresent.")

(defvar epresent--org-buffer nil
  "Original Org-mode buffer")

(defvar epresent--org-restriction nil
  "Original restriction in Org-mode buffer.")

(defvar epresent--org-file nil
  "Temporary Org-mode file used when a narrowed region.")

(defvar epresent-text-scale 3)

(defvar epresent-overlays nil)

(defvar epresent-inline-image-overlays nil)
(defvar epresent-src-fontify-natively nil)
(defvar epresent-hide-emphasis-markers nil)
(defvar epresent-pretty-entities nil)
(defvar epresent-format-latex-scale nil)
(defvar epresent-hide-todos t)
(defvar epresent-hide-tags t)
(defvar epresent-hide-properties t)

(defvar epresent-frame-level 1)
(make-variable-frame-local 'epresent-frame-local) ;; Obsolete function?

(defvar epresent-mode-line nil
  "Set the mode-line format. Hides it when nil")

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
(defun epresent-get-frame-level ()
  "Get the heading level to show as different frames."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (re-search-forward
           "^#\\+EPRESENT_FRAME_LEVEL:[ \t]*\\(.*?\\)[ \t]*$" nil t)
          (string-to-number (match-string 1))
        1))))

(defun epresent-goto-top-level ()
  "Go to the current top level heading containing point."
  (interactive)
  (unless (org-at-heading-p) (outline-previous-heading))
  (let ((level (ignore-errors (org-reduced-level (org-current-level)))))
    (when (and level (> level epresent-frame-level))
      (org-up-heading-all (- level epresent-frame-level)))))

(defun epresent-current-page ()
  "Present the current outline heading."
  (interactive)
  (if (org-current-level)
      (progn
        (epresent-goto-top-level)
        (org-narrow-to-subtree)
        (show-all)
        (hide-body)
        (when (>= (org-reduced-level (org-current-level))
                  epresent-frame-level)
          (org-show-subtree)))
    ;; before first headline -- fold up subtrees as TOC
    (org-cycle '(4))))

(defun epresent-top ()
  "Present the first outline heading."
  (interactive)
  (widen)
  (goto-char (point-min))
  (epresent-current-page))

(defun epresent-next-page ()
  "Present the next outline heading."
  (interactive)
  (epresent-goto-top-level)
  (widen)
  (if (< (or (ignore-errors (org-reduced-level (org-current-level))) 0)
         epresent-frame-level)
      (outline-next-heading)
    (org-get-next-sibling))
  (epresent-current-page))

(defun epresent-previous-page ()
  "Present the previous outline heading."
  (interactive)
  (epresent-goto-top-level)
  (widen)
  (org-content)
  (if (< (or (ignore-errors (org-reduced-level (org-current-level))) 0)
         epresent-frame-level)
      (outline-previous-heading)
    (org-get-last-sibling))
  (epresent-current-page))

(defun epresent-clean-overlays (&optional start end)
  (interactive)
  (let (kept)
    (dolist (ov epresent-overlays)
      (if (or (and start (overlay-start ov) (<= (overlay-start ov) start))
              (and end   (overlay-end   ov) (>= (overlay-end   ov) end)))
          (push ov kept)
        (delete-overlay ov)))
    (setq epresent-overlays kept)))

(defun epresent-quit ()
  "Quit the current presentation."
  (interactive)
  ;; restore the font size
  (text-scale-adjust (/ 1 epresent-text-scale))
  (org-remove-latex-fragment-image-overlays)
  ;; restore the user's Org-mode variables
  (setq org-inline-image-overlays epresent-inline-image-overlays)
  (setq org-src-fontify-natively epresent-src-fontify-natively)
  (setq org-hide-emphasis-markers epresent-hide-emphasis-markers)
  (setq org-pretty-entities epresent-pretty-entities)
  (remove-hook 'org-babel-after-execute-hook 'epresent-refresh)
  (plist-put org-format-latex-options :scale epresent-format-latex-scale)
  (when (string= "EPresent" (frame-parameter nil 'title))
    (delete-frame (selected-frame)))
  (when epresent--org-file
    (kill-buffer (get-file-buffer epresent--org-file))
    (when (file-exists-p epresent--org-file)
      (delete-file epresent--org-file)))
  (when epresent--org-buffer
    (set-buffer epresent--org-buffer))
  (org-mode)
  (if epresent--org-restriction
      (apply #'narrow-to-region epresent--org-restriction)
    (widen))
  ;; delete all epresent overlays
  (epresent-clean-overlays))

(defun epresent-increase-font ()
  "Increase the presentation font size."
  (interactive)
  (dolist (face
           '(epresent-heading-face epresent-content-face epresent-fixed-face))
    (set-face-attribute face nil :height (1+ (face-attribute face :height)))))

(defun epresent-decrease-font ()
  "Decrease the presentation font size."
  (interactive)
  (dolist (face
           '(epresent-heading-face epresent-content-face epresent-fixed-face))
    (set-face-attribute face nil :height (1- (face-attribute face :height)))))

(defun epresent-fontify ()
  "Overlay additional presentation faces to Org-mode."
  (save-excursion
    ;; hide all comments
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*#\\(\\+\\(author\\|title\\|date\\):\\)?.*\n"
            nil t)
      (unless (and (match-string 2)
                   (save-match-data
                     (string-match (regexp-opt '("title" "author" "date"))
                                   (match-string 2))))
        (push (make-overlay (match-beginning 0) (match-end 0)) epresent-overlays)
        (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)))
    ;; page title faces
    (goto-char (point-min))
    (while (re-search-forward "^\\(*+\\)[ \t]*\\(.*\\)$" nil t)
      (push (make-overlay (match-beginning 1) (match-end 1)) epresent-overlays)
      (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)
      (push (make-overlay (match-beginning 2) (match-end 2)) epresent-overlays)
      (if (> (length (match-string 1)) 1)
          (overlay-put (car epresent-overlays) 'face 'epresent-subheading-face)
        (overlay-put (car epresent-overlays) 'face 'epresent-heading-face)))
    ;; fancy bullet points
    (mapc (lambda (p)
            (goto-char (point-min))
            (while (re-search-forward
                    (format "^%s\\(-\\) " (car p)) nil t)
              (push (make-overlay (match-beginning 1) (match-end 1))
                    epresent-overlays)
              (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)
              (overlay-put (car epresent-overlays)
                           'before-string
                           (propertize (cdr p) 'face 'epresent-bullet-face))))
          '(("[ \t]+" . "∘")
            ("" . "•")))
    ;; hide todos
    (when epresent-hide-todos
      (goto-char (point-min))
      (while (re-search-forward org-todo-regexp nil t) 
        (push (make-overlay (match-beginning 1) (1+ (match-end 1)))
              epresent-overlays)
        (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)))
    ;; hide tags
    (when epresent-hide-tags
      (goto-char (point-min))
      (while (re-search-forward 
              (org-re "^\\*+.*?\\([ \t]+:[[:alnum:]_@#%:]+:\\)[ \r\n]") 
              nil t)
        (push (make-overlay (match-beginning 1) (match-end 1)) epresent-overlays)
        (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)))
    ;; hide properties
    (when epresent-hide-properties
      (goto-char (point-min))
      (while (re-search-forward org-drawer-regexp nil t)
        (let ((beg (match-beginning 0))
              (end (re-search-forward
                    "^[ \t]*:END:[ \r\n]*"
                    (save-excursion (outline-next-heading) (point)) t)))
          (push (make-overlay beg end) epresent-overlays)
          (overlay-put (car epresent-overlays) 'invisible 'epresent-hide))))
    (dolist (el '("title" "author" "date"))
      (goto-char (point-min))
      (when (re-search-forward (format "^\\(#\\+%s:\\)[ \t]*\\(.*\\)$" el) nil t)
        (push (make-overlay (match-beginning 1) (match-end 1)) epresent-overlays)
        (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)
        (push (make-overlay (match-beginning 2) (match-end 2)) epresent-overlays)
        (overlay-put
         (car epresent-overlays) 'face (intern (format "epresent-%s-face" el)))))
    ;; inline images
    (org-display-inline-images)))

(defun epresent-refresh ()
  (interactive)
  (epresent-clean-overlays (point-min) (point-max))
  (epresent-fontify))

(defun epresent-flash-cursor ()
  (setq cursor-type 'hollow)
  (sit-for 0.5)
  (setq cursor-type nil))

(defun epresent-next-src-block (&optional arg)
  (interactive "P")
  (org-babel-next-src-block arg)
  (epresent-flash-cursor))

(defun epresent-previous-src-block (&optional arg)
  (interactive "P")
  (org-babel-previous-src-block arg)
  (epresent-flash-cursor))

(defvar epresent-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    ;; line movement
    (define-key map "j" 'scroll-up)
    (define-key map [down] 'scroll-up)
    (define-key map "k" 'scroll-down)
    (define-key map [up] 'scroll-down)
    ;; page movement
    (define-key map " " 'epresent-next-page)
    (define-key map "n" 'epresent-next-page)
    (define-key map "f" 'epresent-next-page)
    (define-key map [right] 'epresent-next-page)
    (define-key map "p" 'epresent-previous-page)
    (define-key map "b" 'epresent-previous-page)
    (define-key map [left] 'epresent-previous-page)
    (define-key map [backspace] 'epresent-previous-page)
    ;; within page functions
    (define-key map "c" 'epresent-next-src-block)
    (define-key map "C" 'epresent-previous-src-block)
    (define-key map "r" 'epresent-refresh)
    (define-key map "g" 'epresent-refresh)
    ;; global controls
    (define-key map "q" 'epresent-quit)
    (define-key map "1" 'epresent-top)
    (define-key map "t" 'epresent-top)
    map)
  "Local keymap for EPresent display mode.")

(define-derived-mode epresent-mode org-mode "EPresent"
  "Lalala."
  (text-scale-adjust 0)
  (text-scale-adjust epresent-text-scale)
  ;; make Org-mode be as pretty as possible
  (setq epresent-inline-image-overlays org-inline-image-overlays)
  (setq epresent-src-fontify-natively org-src-fontify-natively)
  (setq org-src-fontify-natively t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq epresent-hide-emphasis-markers org-hide-emphasis-markers)
  (setq org-hide-emphasis-markers t)
  (setq epresent-pretty-entities org-pretty-entities)
  (setq org-hide-pretty-entities t)
  (setq mode-line-format epresent-mode-line)
  (setq epresent-format-latex-scale (plist-get org-format-latex-options :scale))
  (add-hook 'org-babel-after-execute-hook 'epresent-refresh)
  (let ((org-format-latex-options
         (plist-put org-format-latex-options :scale 4.0)))
    (org-preview-latex-fragment 16))
  ;; fontify the buffer
  (add-to-invisibility-spec '(epresent-hide))
  ;; remove flyspell overlays
  (flyspell-mode-off)
  (epresent-fontify))

;;;###autoload
(defun epresent-run ()
  "Present an Org-mode buffer."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "EPresent can only be used from Org Mode"))
  (setq epresent--org-buffer (current-buffer))
  ;; To present narrowed region use temporary buffer
  (when (and (or (> (point-min) (save-restriction (widen) (point-min)))
                 (< (point-max) (save-restriction (widen) (point-max))))
             (save-excursion (goto-char (point-min)) (org-at-heading-p)))
    (let ((title (nth 4 (org-heading-components))))
      (setq epresent--org-restriction (list (point-min) (point-max)))
      (require 'ox-org)
      (setq epresent--org-file (org-org-export-to-org nil 'subtree))
      (find-file epresent--org-file)
      (goto-char (point-min))
      (insert (format "#+Title: %s\n\n" title))))
  (setq epresent-frame-level (epresent-get-frame-level))
  (epresent--get-frame)
  (epresent-mode)
  (set-buffer-modified-p nil))

;;;###autoload(global-set-key [f12] 'epresent-run)

(provide 'epresent)
;;; epresent.el ends here
