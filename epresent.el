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
;; Emacs >= 23, which has a nice font rendering engine.

;; To use, invoke `epresent-run' in an `org-mode' buffer.  This will
;; make a full-screen frame.  Use n/p to navigate, or q to quit.  Read
;; below for more key bindings.  Each top-level headline becomes a
;; frame in the presentation (configure `EPRESENT_FRAME_LEVEL' to
;; change this default). Org-mode markup is used to nicely display the
;; buffer's contents.

;;; Code:
(require 'org)
(require 'ox)
(require 'ox-latex)
(require 'cl-lib)

(defgroup epresent ()
  "This is a simple presentation mode for Emacs."
  :group 'applications)

(defface epresent-title-face '((t :inherit org-title))
  "Face used for the title of the document during the presentation."
  :group 'epresent)

(defface epresent-heading-face '((t :inherit org-level-1))
  "Face used for the top-level headings in the outline during the presentation."
  :group 'epresent)

(defface epresent-subheading-face '((t :inherit org-level-2))
  "Face used for any non-top-level headings in the outline during the presentation."
  :group 'epresent)

(defface epresent-author-face '((t :inherit org-info))
  "Face used for the author of the document during the presentation."
  :group 'epresent)
(defface epresent-date-face '((t :inherit org-info))
  "Face used for the author of the document during the presentation."
  :group 'epresent)
(defface epresent-bullet-face '((t))
  "Face used for bullets during the presentation."
  :group 'epresent)

(defface epresent-hidden-face '((t :invisible t))
  "Face used for hidden elements during the presentation."
  :group 'epresent)

(defvar epresent--frame nil
  "Frame for EPresent.")

(defvar epresent--org-buffer nil
  "Original Org-mode buffer.")

(defvar epresent--org-restriction nil
  "Original restriction in Org-mode buffer.")

(defvar epresent--org-file nil
  "Temporary Org-mode file used when a narrowed region.")

(defvar epresent--possibly-modified nil
  "Set to non-nil when the `epresent--org-file' might be modified.")

(defcustom epresent-face-attributes '((default :height 500))
  "Base font size in 1/10 point."
  :type '(alist :key-type symbol :value-type (plist))
  :group 'epresent)

(defcustom epresent-pretty-entities 'org
  "Use unicode versions of entities when possible.
Org falls back to the `org-pretty-entities` value."
  :type '(choice (const :tag "Inherit Org’s value"  org)
                 (const :tag "Use Unicode Entities" t)
                 (const :tag "Use plain entities"   nil))
  :group 'epresent)

(defvar epresent--org-pretty-entities nil)

(defvar epresent-overlays nil)

(defvar epresent-inline-image-overlays nil)
(defvar epresent-src-fontify-natively nil)
(defvar epresent-hide-emphasis-markers nil)
(defvar epresent-outline-ellipsis nil)
(defvar epresent-border-width 20)

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

(defvar epresent-page-number 0)

(defvar epresent-frame-level 1)

(defcustom epresent-header-line nil
  "Set the header-line format. Hides it when nil."
  :group 'epresent)

(defcustom epresent-mode-line '(:eval (int-to-string epresent-page-number))
  "Set the mode-line format.  Hides it when nil."
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

(defvar epresent-src-block-toggle-state nil)

(defun epresent--get-frame ()
  (unless (frame-live-p epresent--frame)
    (setq epresent--frame
          (make-frame `((minibuffer . nil)
                        (title . "EPresent")
                        (menu-bar-lines . 0)
                        (tool-bar-lines . 0)
                        (vertical-scroll-bars . nil)
                        (left-fringe . 0)
                        (right-fringe . 0)
                        (internal-border-width . ,epresent-border-width)
                        (cursor-type . nil))))
    (select-frame-set-input-focus epresent--frame)
    (toggle-frame-fullscreen))
  (raise-frame epresent--frame)
  (select-frame-set-input-focus epresent--frame)
  epresent--frame)

;; TODO: How does Org not already have a mapping from file-option to value?
(defun epresent--get-file-option (file-option)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (concat "^#\\+"
                                       (replace-regexp-in-string
                                        "-"
                                        "_"
                                        (symbol-name file-option)
                                        t
                                        t)
                                       ":[ \t]*\\(.*?\\)[ \t]*$")
                               nil
                               t)
        (match-string 1)))))

(defun epresent-get-frame-level ()
  "Get the heading level to show as different frames."
  (interactive)
  (string-to-number (or (epresent--get-file-option 'epresent-frame-level)
                        "1")))

(defun epresent-get-header-line ()
  "Get the presentation-specific header-line."
  (interactive)
  (car (read-from-string (or (epresent--get-file-option 'epresent-header-line)
                             (prin1-to-string epresent-header-line)))))

(defun epresent-get-mode-line ()
  "Get the presentation-specific mode-line."
  (interactive)
  (car (read-from-string (or (epresent--get-file-option 'epresent-mode-line)
                             (prin1-to-string epresent-mode-line)))))

(defun epresent-get-face-attributes ()
  "Get the presentation-specific face attributes."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (append
       epresent-face-attributes
       (when (re-search-forward
              "^#\\+EPRESENT_FACE_ATTRIBUTES:[ \t]*\\(.*?\\)[ \t]*$" nil t)
         (car (read-from-string (match-string 1))))))))

(defun epresent-goto-top-level ()
  "Go to the current top level heading containing point."
  (interactive)
  (unless (org-at-heading-p) (outline-previous-heading))
  (let ((level (ignore-errors (org-reduced-level (org-current-level)))))
    (when (and level (> level epresent-frame-level))
      (org-up-heading-all (- level epresent-frame-level)))))

(defun epresent-jump-to-page (num)
  "Jump directly to page NUM in the presentation."
  (interactive "npage number: ")
  (epresent-top)
  (dotimes (_ (1- num)) (epresent-next-page)))

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
          (org-show-subtree)
          (let ((epresent-src-block-toggle-state
                 (if epresent-src-blocks-visible :show :hide)))
            (epresent-toggle-hide-src-blocks))))
    ;; before first headline -- fold up subtrees as TOC
    (org-cycle '(4))))

(defun epresent-top ()
  "Present the first outline heading."
  (interactive)
  (widen)
  (goto-char (point-min))
  (setq epresent-page-number 1)
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
  (setq epresent-page-number (1+ epresent-page-number))
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
  (when (< 1 epresent-page-number)
    (setq epresent-page-number (1- epresent-page-number)))
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
  (run-hooks 'epresent-stop-presentation-hook)
  (org-remove-latex-fragment-image-overlays)
  ;; restore the user's Org-mode variables
  (remove-hook 'org-src-mode-hook 'epresent-setup-src-edit)
  (setq org-inline-image-overlays epresent-inline-image-overlays)
  (setq org-src-fontify-natively epresent-src-fontify-natively)
  (setq org-hide-emphasis-markers epresent-hide-emphasis-markers)
  (set-display-table-slot standard-display-table
                          'selective-display epresent-outline-ellipsis)
  (unless (eq epresent-pretty-entities 'org)
    (setq org-pretty-entities epresent--org-pretty-entities))
  (remove-hook 'org-babel-after-execute-hook 'epresent-refresh)
  (when (string= "EPresent" (frame-parameter nil 'title))
    (delete-frame (selected-frame)))
  (when epresent--org-file
    (kill-buffer (get-file-buffer epresent--org-file))
    (when (file-exists-p epresent--org-file)
      (when epresent--possibly-modified
        (let ((temp (make-temp-file "epresent" nil ".org")))
          (copy-file epresent--org-file temp 'overwrite)
          (message "Presentation edits saved to %S" temp)))
      (delete-file epresent--org-file)))
  (when epresent--org-buffer
    (set-buffer epresent--org-buffer))
  (org-mode)
  (if epresent--org-restriction
      (apply #'narrow-to-region epresent--org-restriction)
    (widen))
  (hack-local-variables)
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
              epresent-overlays)
        (overlay-put (car epresent-overlays) 'invisible 'epresent-hide))
       (t (push (make-overlay (match-beginning 0) (match-end 0))
                epresent-overlays)
          (overlay-put (car epresent-overlays) 'invisible 'epresent-hide))))
    ;; page title faces
    (goto-char (point-min))
    (while (re-search-forward "^\\(*+\\)\\([ \t]+\\)\\(.*\\)$" nil t)
      (push (make-overlay (match-beginning 1) (or (match-end 2)
                                                  (match-end 1)))
            epresent-overlays)
      (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)
      (push (make-overlay (match-beginning 3) (match-end 3)) epresent-overlays)
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
      (while (re-search-forward org-todo-line-regexp nil t)
        (when (match-string 2)
          (push (make-overlay (match-beginning 2) (1+ (match-end 2)))
                epresent-overlays)
          (overlay-put (car epresent-overlays) 'invisible 'epresent-hide))))
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
      (when (re-search-forward (format "^\\(#\\+%s:[ \t]*\\)[ \t]*\\(.*\\)$" el) nil t)
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

(defun epresent-toggle-hide-src-blocks (&optional arg)
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
                 (unless (and epresent-src-block-toggle-state
                              (eq epresent-src-block-toggle-state :hide))
                   (progn
                     (mapc #'delete-overlay ovs)
                     (setq epresent-overlays
                           (cl-set-difference epresent-overlays ovs))))
               (unless (and epresent-src-block-toggle-state
                            (eq epresent-src-block-toggle-state :show))
                 (progn
                   (push (make-overlay beg end) epresent-overlays)
                   (overlay-put (car epresent-overlays)
                                'epresent-hidden-src-block t)
                   (overlay-put (car epresent-overlays)
                                'invisible 'epresent-hide))))))))
    (if arg (toggle)               ; only toggle the current src block
      (save-excursion              ; toggle all source blocks
        (goto-char (point-min))
        (while (re-search-forward org-babel-src-block-regexp nil t)
          (goto-char (1- (match-end 5)))
          (toggle))))
    (redraw-display)))

(defun epresent-toggle-hide-src-block (&optional arg)
  (interactive "P")
  (epresent-toggle-hide-src-blocks t))

(defun epresent-increase-inner-border ()
  (interactive)
  (modify-frame-parameters
   epresent--frame
   `((internal-border-width . ,(incf epresent-border-width 10)))))

(defun epresent-decrease-inner-border ()
  (interactive)
  (modify-frame-parameters
   epresent--frame
   `((internal-border-width . ,(decf epresent-border-width 10)))))

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
    (define-key map "a" 'epresent-increase-inner-border)
    (define-key map ";" 'epresent-decrease-inner-border)

    map)
  "Local keymap for EPresent display mode.")

(define-derived-mode epresent-mode org-mode "EPresent"
  "Lalala."
  ;; make Org-mode be as pretty as possible
  (add-hook 'org-src-mode-hook 'epresent-setup-src-edit)
  (setq epresent-inline-image-overlays org-inline-image-overlays)
  (setq epresent-src-fontify-natively org-src-fontify-natively)
  (setq org-src-fontify-natively t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq epresent-hide-emphasis-markers org-hide-emphasis-markers)
  (setq org-hide-emphasis-markers t)
  (setq epresent-outline-ellipsis
        (display-table-slot standard-display-table 'selective-display))
  (set-display-table-slot standard-display-table 'selective-display [32])
  (unless (eq epresent-pretty-entities 'org)
    (setq epresent--org-pretty-entities org-pretty-entities
          org-pretty-entities epresent-pretty-entities))
  (setq header-line-format (epresent-get-header-line)
        mode-line-format (epresent-get-mode-line))
  (add-hook 'org-babel-after-execute-hook 'epresent-refresh)
  (let ((org-format-latex-options
         (plist-put (copy-tree org-format-latex-options)
		    :scale epresent-format-latex-scale)))
    (org-preview-latex-fragment '(16)))
  (mapc (lambda (pair)
          (let ((face (car pair))
                (attributes (cdr pair)))
            (apply #'set-face-attribute face epresent--frame attributes)))
        (epresent-get-face-attributes))
  ;; fontify the buffer
  (add-to-invisibility-spec '(epresent-hide))
  ;; remove flyspell overlays
  (flyspell-mode-off)
  (epresent-fontify))

(defvar epresent-edit-map (let ((map (copy-keymap org-mode-map)))
                            (define-key map [f5] 'epresent-refresh)
                            map)
  "Local keymap for editing EPresent presentations.")

(defun epresent-edit-text (&optional arg)
  "Write in EPresent presentation."
  (interactive "p")
  (when epresent--org-file (setq epresent--possibly-modified t))
  (lexical-let
      ((prior-cursor-type (cdr (assoc 'cursor-type (frame-parameters)))))
    (set-frame-parameter nil 'cursor-type t)
    (use-local-map epresent-edit-map)
    (set-transient-map
     epresent-edit-map
     (lambda () (not (equal [f5] (this-command-keys))))
     (lambda ()
       (use-local-map epresent-mode-map)
       (set-frame-parameter nil 'cursor-type prior-cursor-type)))))

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
      (find-file epresent--org-file)))
  (setq epresent-frame-level (epresent-get-frame-level))
  (epresent--get-frame)
  (epresent-mode)
  (set-buffer-modified-p nil)
  (run-hooks 'epresent-start-presentation-hook))

(define-key org-mode-map [f5]  'epresent-run)
(define-key org-mode-map [f12] 'epresent-run)

(provide 'epresent)
;;; epresent.el ends here
