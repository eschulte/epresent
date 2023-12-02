;;; epresent-evil.el --- Epresent support for EVIL

;; Copyright (C) 2008 Tom Tromey <tromey@redhat.com>
;;               2010 Eric Schulte <schulte.eric@gmail.com>

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

;; Add ‘epresent-evil-bindings’ to ‘evil-mode-hook’ to make the Epresent key
;; bindings available in EVIL.

;;; Code:

(require 'epresent)
(require 'evil)

;;;###autoload
(defun epresent-evil-bindings ()
  "Support the Epresent bindings in ‘evil-mode’.
Also add h and l for previous and next page respectively."
  (let ((map epresent-mode-map))
    (evil-define-key 'normal map "j" 'scroll-up)
    (evil-define-key 'normal map [down] 'scroll-up)
    (evil-define-key 'normal map "k" 'scroll-down)
    (evil-define-key 'normal map [up] 'scroll-down)

    (evil-define-key 'normal map " " 'epresent-next-page)
    (evil-define-key 'normal map "n" 'epresent-next-page)
    (evil-define-key 'normal map "l" 'epresent-next-page)
    (evil-define-key 'normal map "f" 'epresent-next-page)
    (evil-define-key 'normal map [right] 'epresent-next-page)
    (evil-define-key 'normal map [next] 'epresent-next-page)

    (evil-define-key 'normal map "p" 'epresent-previous-page)
    (evil-define-key 'normal map "b" 'epresent-previous-page)
    (evil-define-key 'normal map "h" 'epresent-previous-page)
    (evil-define-key 'normal map [left] 'epresent-previous-page)
    (evil-define-key 'normal map [prior] 'epresent-previous-page)
    (evil-define-key 'normal map [backspace] 'epresent-previous-page)

    (evil-define-key 'normal map "v" 'epresent-jump-to-page)

    (evil-define-key 'normal map "c" 'epresent-next-src-block)
    (evil-define-key 'normal map "C" 'epresent-next-src-block)
    (evil-define-key 'normal map "e" 'org-edit-src-code)
    ;; Another [f10] exits edit mode.
    (evil-define-key 'normal map [f10] 'epresent-edit-text)

    (evil-define-key 'normal map "x" 'epresent-execute-src-block)

    (evil-define-key 'normal map "q" 'epresent-quit)
    (evil-define-key 'normal map "s" 'epresent-toggle-hide-src-blocks)
    (evil-define-key 'normal map "S" 'epresent-toggle-hide-src-block)
    (evil-define-key 'normal map "1" 'epresent-top)
    (evil-define-key 'normal map "t" 'epresent-top)
    (evil-define-key 'normal map "a" 'epresent-increase-inner-border)
    (evil-define-key 'normal map ";" 'epresent-decrease-inner-border))

  (defadvice epresent-run (after epresent-evil)
    "In evil-mode, force normal state for keybindings."
    (evil-force-normal-state)))

(provide 'epresent-evil)
;;; epresent-evil.el ends here
