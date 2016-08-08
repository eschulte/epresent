;;; -*- lexical-binding: t -*-
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

;; This is a simple presentation mode for Emacs. It works best in
;; Emacs >= 23, which has a nice font rendering engine.

;; To use, invoke `epresent-run' in an `org-mode' buffer. This will
;; make a full-screen frame. Use n/p to navigate, or q to quit. Read
;; below for more key bindings. Each top-level headline becomes a
;; frame in the presentation (configure `EPRESENT_FRAME_LEVEL' to
;; change this default). Org-mode markup is used to nicely display the
;; buffer's contents.

;;; Code:
(require 'org)
(require 'ox)
(require 'ox-latex)
(require 'cl-lib)

(defgroup epresent () "This is a simple presentation mode for Emacs.")

(defface epresent-title-face
  '((t :weight bold :height 360 :underline t :inherit variable-pitch))
  "Face used for the title of the document during the presentation."
  :group 'epresent)

(defface epresent-heading-face
  '((t :weight bold :height 270 :underline t :inherit variable-pitch))
  "Face used for the top-level headings in the outline during the presentation."
  :group 'epresent)

(defface epresent-subheading-face
  '((t :weight bold :height 240 :inherit variable-pitch))
  "Face used for any non-top-level headings in the outline during the presentation."
  :group 'epresent)

(defface epresent-author-face
  '((t :height 1.6 :inherit variable-pitch))
  "Face used for the author of the document during the presentation."
  :group 'epresent)

(defface epresent-bullet-face
  '((t :weight bold :height 1.4 :underline nil :inherit variable-pitch))
  "Face used for bullets during the presentation."
  :group 'epresent)

(defface epresent-hidden-face
  '((t :invisible t))
  "Face used for hidden elements during the presentation."
  :group 'epresent)

(defcustom epresent-text-size 500
  "Text size when presenting"
  :type 'number
  :group 'epresent)

(defcustom epresent-format-latex-scale 4
  "A scaling factor for the size of the images generated from LaTeX."
  :type 'number
  :group 'epresent)
(defcustom epresent-hide-todos t
  "Whether or not to hide TODOs during the presentation."
  :type 'boolean
  :group 'epresent)
(defcustom epresent-hide-tags t
  "Whether or not to hide tags during the presentation."
  :type 'boolean
  :group 'epresent)
(defcustom epresent-hide-properties t
  "Whether or not to hide properties during the presentation."
  :type 'boolean
  :group 'epresent)

(defcustom epresent-mode-line
  (lambda (presentation)
    `(:eval (int-to-string (epresent-page-number ,presentation))))
  "A function that evaluates to a mode-line format. It must take a single
parameter, which is the presentation the mode-line is being applied to."
  :type 'function
  :group 'epresent)

(defcustom epresent-src-blocks-visible t
  "If non-nil source blocks are initially visible on slide change.
If nil then source blocks are initially hidden on slide change."
  :type 'boolean
  :group 'epresent)

(defcustom epresent-start-presentation-hook nil
  "Hook run after starting a presentation."
  :type 'hook
  :group 'epresent)
(defcustom epresent-stop-presentation-hook nil
  "Hook run before stopping a presentation."
  :type 'hook
  :group 'epresent)

(defvar epresent--default-presentation nil
  "The presentation to use if none is provided.")

(defvar epresent--org-bindings
  '((org-fontify-quote-and-verse-blocks . t)
    (org-hide-emphasis-markers          . t)
    (org-inline-image-overlays          . nil)
    (org-pretty-entities                . nil)
    (org-src-fontify-natively           . t))
  "Mapping of Epresent-specific values for Org variables.")

(defun epresent--get-presentation ()
  (let ((presentation (or epresent--default-presentation
                          (setq epresent--default-presentation
                                (make-instance 'epresent-presentation)))))
    ;; FIXME: Should set this as initform, but that is getting evaluated at
    ;;        load-time.
    (set-slot-value presentation 'frame
                    (make-frame '((minibuffer . nil)
                                  (title . "EPresent")
                                  (fullscreen . fullboth)
                                  (menu-bar-lines . 0)
                                  (tool-bar-lines . 0)
                                  (vertical-scroll-bars . nil)
                                  (left-fringe . 0)
                                  (right-fringe . 0)
                                  (internal-border-width . 20)
                                  (cursor-type . nil))))
    (raise-frame (slot-value presentation 'frame))
    (select-frame-set-input-focus (slot-value presentation 'frame))
    presentation))

;; functions
(defun epresent--get-frame-level (presentation)
  "Get the heading level to show as different frames."
  (interactive)
  (with-current-buffer (slot-value presentation 'org-buffer)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (if (re-search-forward
             "^#\\+EPRESENT_FRAME_LEVEL:[ \t]*\\(.*?\\)[ \t]*$" nil t)
            (string-to-number (match-string 1))
          1)))))

(defun epresent--get-mode-line (presentation)
  "Get the presentation-specific mode-line."
  (interactive)
  (with-current-buffer (slot-value presentation 'org-buffer)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (funcall (if (re-search-forward
                      "^#\\+EPRESENT_MODE_LINE:[ \t]*\\(.*?\\)[ \t]*$" nil t)
                     (car (read-from-string (match-string 1)))
                   epresent-mode-line)
                 presentation)))))

(defun epresent--goto-top-level (presentation)
  "Go to the current top level heading containing point."
  (interactive)
  (unless (org-at-heading-p) (outline-previous-heading))
  (let ((level (ignore-errors (org-reduced-level (org-current-level))))
        (frame-level (slot-value presentation 'frame-level)))
    (when (and level (> level frame-level))
      (org-up-heading-all (- level frame-level)))))

(cl-defun epresent-jump-to-page
    (num &optional (presentation epresent--default-presentation))
  "Jump directly to a particular page in the presentation."
  (interactive "npage number: ")
  (epresent-top presentation)
  (dotimes (_ (1- num)) (epresent-next-page presentation)))

(defun epresent--current-page (presentation)
  "Present the current outline heading."
  (interactive)
  (if (org-current-level)
      (progn
        (epresent--goto-top-level presentation)
        (org-narrow-to-subtree)
        (show-all)
        (hide-body)
        (when (>= (org-reduced-level (org-current-level))
                  (slot-value presentation 'frame-level))
          (org-show-subtree)
          (epresent-toggle-hide-src-blocks nil
                                           presentation
                                           (if epresent-src-blocks-visible
                                               :show
                                             :hide))))
    ;; before first headline -- fold up subtrees as TOC
    (org-cycle '(4))))

(cl-defun epresent-top (&optional (presentation epresent--default-presentation))
  "Present the first outline heading."
  (interactive)
  (widen)
  (goto-char (point-min))
  (set-slot-value presentation 'page-number 1)
  (epresent--current-page presentation))

(cl-defun epresent-next-page
    (&optional (presentation epresent--default-presentation))
  "Present the next outline heading."
  (interactive)
  (epresent--goto-top-level presentation)
  (widen)
  (if (< (or (ignore-errors (org-reduced-level (org-current-level))) 0)
         (slot-value presentation 'frame-level))
      (outline-next-heading)
    (org-get-next-sibling))
  (incf (slot-value presentation 'page-number))
  (epresent--current-page presentation))

(cl-defun epresent-previous-page
    (&optional (presentation epresent--default-presentation))
  "Present the previous outline heading."
  (interactive)
  (epresent--goto-top-level presentation)
  (widen)
  (org-content)
  (if (< (or (ignore-errors (org-reduced-level (org-current-level))) 0)
         (slot-value presentation 'frame-level))
      (outline-previous-heading)
    (org-get-last-sibling))
  (let ((page-number (slot-value presentation 'page-number)))
    (when (< 1 page-number)
      (decf (slot-value presentation 'page-number))))
  (epresent--current-page presentation))

(defun epresent--clean-overlays (presentation &optional start end)
  (interactive)
  (let (kept)
    (dolist (ov (slot-value presentation 'overlays))
      (if (or (and start (overlay-start ov) (<= (overlay-start ov) start))
              (and end   (overlay-end   ov) (>= (overlay-end   ov) end)))
          (push ov kept)
        (delete-overlay ov)))
    (set-slot-value presentation 'overlays kept)))

(defun epresent--swap-bindings (bindings)
  (mapcar (lambda (binding)
            (let ((val (cdr binding)))
              (prog1 (cons (car binding) (symbol-value (car binding)))
                (set (car binding) val))))
          bindings))

(cl-defun epresent-quit
    (&optional (presentation epresent--default-presentation))
  "Quit the current presentation."
  (interactive)
  (run-hooks 'epresent-stop-presentation-hook)
  (org-remove-latex-fragment-image-overlays)
  ;; restore the user's Org-mode variables
  (remove-hook 'org-src-mode-hook 'epresent-setup-src-edit)
  (epresent--swap-bindings epresent--org-bindings)
  (set-display-table-slot standard-display-table 'selective-display
                          (slot-value presentation 'outline-ellipsis))
  (remove-hook 'org-babel-after-execute-hook 'epresent-refresh)
  (delete-frame (slot-value presentation 'frame))
  (let* ((org-file (slot-value presentation 'org-file))
         (org-buffer (slot-value presentation 'org-buffer))
         (org-restriction (slot-value presentation 'org-restriction)))
    (when org-file
      (kill-buffer (get-file-buffer org-file))
      (when (file-exists-p org-file)
        (when (slot-value presentation 'possibly-modified)
          (let ((temp (make-temp-file "epresent" nil ".org")))
            (copy-file org-file temp 'overwrite)
            (message "Presentation edits saved to %S" temp)))
        (delete-file org-file)))
    (when org-buffer
      (set-buffer org-buffer))
    (org-mode)
    (if org-restriction
        (apply #'narrow-to-region org-restriction)
      (widen)))
  (hack-local-variables)
  ;; delete all epresent overlays
  (epresent--clean-overlays presentation)
  (when (eq presentation epresent--default-presentation)
    (setq epresent--default-presentation nil)))

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

(defun epresent--fontify (presentation)
  "Overlay additional presentation faces to Org-mode."
  (let ((overlays (slot-value presentation 'overlays)))
    (save-excursion
      ;; hide all comments
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*#\\(\\+\\(author\\|title\\|date\\):\\)?.*\n"
              nil t)
        (cond
         ((and (match-string 2)
               (save-match-data
                 (string-match (regexp-opt '("title" "author" "date"))
                               (match-string 2)))))
         ((and (match-string 2)
               (save-match-data
                 (string-match org-babel-results-keyword (match-string 2))))
          ;; This pulls back the end of the hidden overlay by one to
          ;; avoid hiding image results of code blocks.  I'm not sure
          ;; why this is required, or why images start on the preceding
          ;; newline, but not knowing why doesn't make it less true.
          (push (make-overlay (match-beginning 0) (1- (match-end 0)))
                (slot-value presentation 'overlays))
          (overlay-put (car (slot-value presentation 'overlays))
                       'invisible 'epresent-hide))
         (t (push (make-overlay (match-beginning 0) (match-end 0))
                  (slot-value presentation 'overlays))
            (overlay-put (car (slot-value presentation 'overlays))
                         'invisible 'epresent-hide))))
      ;; page title faces
      (goto-char (point-min))
      (while (re-search-forward "^\\(*+\\)\\([ \t]+\\)\\(.*\\)$" nil t)
        (push (make-overlay (match-beginning 1) (or (match-end 2)
                                                    (match-end 1)))
              (slot-value presentation 'overlays))
        (overlay-put (car (slot-value presentation 'overlays))
                     'invisible 'epresent-hide)
        (push (make-overlay (match-beginning 3) (match-end 3))
              (slot-value presentation 'overlays))
        (if (> (length (match-string 1)) 1)
            (overlay-put (car (slot-value presentation 'overlays))
                         'face 'epresent-subheading-face)
          (overlay-put (car (slot-value presentation 'overlays))
                       'face 'epresent-heading-face)))
      ;; fancy bullet points
      (mapc (lambda (p)
              (goto-char (point-min))
              (while (re-search-forward
                      (format "^%s\\(-\\) " (car p)) nil t)
                (push (make-overlay (match-beginning 1) (match-end 1))
                      (slot-value presentation 'overlays))
                (overlay-put (car (slot-value presentation 'overlays))
                             'invisible 'epresent-hide)
                (overlay-put (car (slot-value presentation 'overlays))
                             'before-string
                             (propertize (cdr p) 'face 'epresent-bullet-face))))
            '(("[ \t]+" . "∘")
              ("" . "•")))
      ;; hide todos
      (when epresent-hide-todos
        (goto-char (point-min))
        (while (re-search-forward org-todo-line-regexp nil t)
          (when (match-string 2)
            (push (make-overlay (match-beginning 2) (1+ (match-end 2)))
                  (slot-value presentation 'overlays))
            (overlay-put (car (slot-value presentation 'overlays))
                         'invisible 'epresent-hide))))
      ;; hide tags
      (when epresent-hide-tags
        (goto-char (point-min))
        (while (re-search-forward
                (org-re "^\\*+.*?\\([ \t]+:[[:alnum:]_@#%:]+:\\)[ \r\n]")
                nil t)
          (push (make-overlay (match-beginning 1) (match-end 1))
                (slot-value presentation 'overlays))
          (overlay-put (car (slot-value presentation 'overlays))
                       'invisible 'epresent-hide)))
      ;; hide properties
      (when epresent-hide-properties
        (goto-char (point-min))
        (while (re-search-forward org-drawer-regexp nil t)
          (let ((beg (match-beginning 0))
                (end (re-search-forward
                      "^[ \t]*:END:[ \r\n]*"
                      (save-excursion (outline-next-heading) (point)) t)))
            (push (make-overlay beg end) (slot-value presentation 'overlays))
            (overlay-put (car (slot-value presentation 'overlays))
                         'invisible 'epresent-hide))))
      (dolist (el '("title" "author" "date"))
        (goto-char (point-min))
        (when (re-search-forward (format "^\\(#\\+%s:[ \t]*\\)[ \t]*\\(.*\\)$" el) nil t)
          (push (make-overlay (match-beginning 1) (match-end 1))
                (slot-value presentation 'overlays))
          (overlay-put (car (slot-value presentation 'overlays))
                       'invisible 'epresent-hide)
          (push (make-overlay (match-beginning 2) (match-end 2))
                (slot-value presentation 'overlays))
          (overlay-put (car (slot-value presentation 'overlays))
                       'face (intern (format "epresent-%s-face" el)))))
      ;; inline images
      (org-display-inline-images))))

(cl-defun epresent-refresh
    (&optional (presentation epresent--default-presentation))
  (interactive)
  (epresent--clean-overlays presentation (point-min) (point-max))
  (epresent--fontify presentation))

(defun epresent-setup-src-edit ()
  (setq cursor-type 'box))

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

(defun epresent-toggle-hide-src-blocks
    (&optional arg
               (presentation epresent--default-presentation)
               src-block-toggle-state)
  (interactive "P")
  (cl-labels
      ((boundaries ()
                   (let ((head (org-babel-where-is-src-block-head)))
                     (if head
                         (save-excursion
                           (goto-char head)
                           (looking-at org-babel-src-block-regexp)
                           (list (match-beginning 5) (match-end 5)))
                       (error "no source block to hide at %d" (point)))))
       (toggle ()
         (cl-destructuring-bind (beg end) (boundaries)
           (let ((ovs (cl-remove-if-not
                       (lambda (ov) (overlay-get ov 'epresent-hidden-src-block))
                       (overlays-at beg))))
             (if ovs
                 (unless (eq src-block-toggle-state :hide)
                   (progn
                     (mapc #'delete-overlay ovs)
                     (set-slot-value presentation 'overlays
                                     (cl-set-difference
                                      (slot-value presentation 'overlays)
                                      ovs))))
               (unless (eq src-block-toggle-state :show)
                 (progn
                   (push (make-overlay beg end) (slot-value presentation 'overlays))
                   (overlay-put (car (slot-value presentation 'overlays))
                                'epresent-hidden-src-block t)
                   (overlay-put (car (slot-value presentation 'overlays))
                                'invisible 'epresent-hide))))))))
    (if arg (toggle)               ; only toggle the current src block
      (save-excursion              ; toggle all source blocks
        (goto-char (point-min))
        (while (re-search-forward org-babel-src-block-regexp nil t)
          (goto-char (1- (match-end 5)))
          (toggle))))
    (redraw-display)))

(defun epresent-toggle-hide-src-block
    (&optional arg (presentation epresent--default-presentation))
  (interactive "P")
  (epresent-toggle-hide-src-blocks t presentation))

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
    (define-key map [next] 'epresent-next-page)
    (define-key map "p" 'epresent-previous-page)
    (define-key map "b" 'epresent-previous-page)
    (define-key map [left] 'epresent-previous-page)
    (define-key map [prior] 'epresent-previous-page)
    (define-key map [backspace] 'epresent-previous-page)
    (define-key map "v" 'epresent-jump-to-page)
    ;; within page functions
    (define-key map "c" 'epresent-next-src-block)
    (define-key map "C" 'epresent-previous-src-block)
    (define-key map "e" 'org-edit-src-code)
    (define-key map [f5] 'epresent-edit-text) ; Another [f5] exits edit mode.
    (define-key map "x" 'org-babel-execute-src-block)
    (define-key map "r" 'epresent-refresh)
    (define-key map "g" 'epresent-refresh)
    ;; global controls
    (define-key map "q" 'epresent-quit)
    (define-key map "1" 'epresent-top)
    (define-key map "s" 'epresent-toggle-hide-src-blocks)
    (define-key map "S" 'epresent-toggle-hide-src-block)
    (define-key map "t" 'epresent-top)
    map)
  "Local keymap for EPresent display mode.")

(defclass epresent-presentation ()
  ((page-number :initform 1 :reader epresent-page-number :type integer)
   (frame)
   (frame-level :initform 1 :type integer)
   (org-buffer :initform nil :documentation "Original Org-mode buffer")
   (org-restriction :initform nil
                    :documentation "Original restriction in Org-mode buffer.")
   (org-file :initform nil
             :documentation "Temporary Org-mode file used when a narrowed region.")
   (possibly-modified :initform nil
                      :documentation "Set to non-nil when ORG-FILE might be modified.")
   (overlays :initform nil)
   (outline-ellipsis :initform nil))
  :documentation "An EPresent presentation.")

(define-derived-mode epresent-mode org-mode "EPresent"
  "Lalala."
  ;; make Org-mode be as pretty as possible
  (add-hook 'org-src-mode-hook 'epresent-setup-src-edit)
  (epresent--swap-bindings epresent--org-bindings)
  (set-slot-value epresent--default-presentation 'outline-ellipsis
                  (display-table-slot standard-display-table
                                      'selective-display))
  (set-display-table-slot standard-display-table 'selective-display [32])
  (setq mode-line-format
        (epresent--get-mode-line epresent--default-presentation))
  (add-hook 'org-babel-after-execute-hook 'epresent-refresh)
  (let ((org-format-latex-options
         (plist-put (copy-tree org-format-latex-options)
		    :scale epresent-format-latex-scale)))
    (org-preview-latex-fragment '(16)))
  (set-face-attribute 'default
                      (slot-value epresent--default-presentation 'frame)
                      :height epresent-text-size)
  ;; fontify the buffer
  (add-to-invisibility-spec '(epresent-hide))
  ;; remove flyspell overlays
  (flyspell-mode-off)
  (epresent--fontify epresent--default-presentation))

(defvar epresent-edit-map (let ((map (copy-keymap org-mode-map)))
                            (define-key map [f5] 'epresent-refresh)
                            map)
  "Local keymap for editing EPresent presentations.")

(cl-defun epresent-edit-text
    (&optional arg (presentation epresent--default-presentation))
  "Write in EPresent presentation."
  (interactive "p")
  (when (slot-value presentation 'org-file)
    (set-slot-value presentation 'possibly-modified t))
  (let* ((frame (slot-value presentation 'frame))
         (prior-cursor-type (frame-parameter frame 'cursor-type)))
    (set-frame-parameter frame 'cursor-type t)
    (use-local-map epresent-edit-map)
    (set-transient-map
     epresent-edit-map
     (lambda () (not (equal [f5] (this-command-keys))))
     (lambda ()
       (use-local-map epresent-mode-map)
       (set-frame-parameter frame 'cursor-type prior-cursor-type)))))

;;;###autoload
(defun epresent-run ()
  "Present an Org-mode buffer."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "EPresent can only be used from Org Mode"))
  (let ((presentation (epresent--get-presentation)))
    (set-slot-value presentation 'org-buffer (current-buffer))
    ;; To present narrowed region use temporary buffer
    (when (and (or (> (point-min) (save-restriction (widen) (point-min)))
                   (< (point-max) (save-restriction (widen) (point-max))))
               (save-excursion (goto-char (point-min)) (org-at-heading-p)))
      (let ((title (nth 4 (org-heading-components))))
        (set-slot-value presentation 'org-restriction
                        (list (point-min) (point-max)))
        (require 'ox-org)
        (set-slot-value presentation 'org-file
                        (org-org-export-to-org nil 'subtree))
        (find-file (slot-value presentation 'org-file))
        (goto-char (point-min))
        (insert (format "#+Title: %s\n\n" title))))
    (set-slot-value presentation 'frame-level (epresent-get-frame-level)))
  (epresent-mode)
  (set-buffer-modified-p nil)
  (run-hooks 'epresent-start-presentation-hook))

(define-key org-mode-map [f5]  'epresent-run)
(define-key org-mode-map [f12] 'epresent-run)

(provide 'epresent)
;;; epresent.el ends here
