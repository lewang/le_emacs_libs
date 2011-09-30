;;; scf-mode.el --- minor-mode to shorten file-names in compilation derived major-modes

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: minor-mode to shorten file-names in compilation derived major-modes
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Sat Oct  1 03:07:18 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Sat Oct  1 03:14:00 2011 (+0800)
;;           By: Le Wang
;;     Update #: 3
;;          URL: https://github.com/lewang/le_emacs_libs/blob/master/scf-mode.el
;; Keywords: compilation
;; Compatibility: Emacs23.3+

;;; Installation:

;;    (require 'scf-mode)
;;    (define-key grep-mode-map [(s)] 'scf-mode)
;;
;; Optional:
;;
;;    (add-hook 'grep-mode-hook (lambda () (scf-mode 1)))
;;

;;; Commentary:

;; Shorten all file-name targets to the basename without directories for
;; easier reading.
;;
;; I only show how to install in `grep-mode', but scf-mode should work for all
;; compilation-mode derived major-modes.
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'grep)

(provide 'scf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scf stands for shorten-compilation-filename ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar scf-invisible-overlays nil)
(add-to-invisibility-spec 'scf)

(defun scf-mode-is-compilation (&optional mode)
  (setq mode (or mode major-mode))
  (memq 'compilation-mode (loop for parent = major-mode then (get parent 'derived-mode-parent)
                                while parent
                                collect parent into parents
                                finally return parents)))

(defun scf-add-invisible-overlay (start end)
  "Add an overlay from `start' to `end' in the current buffer.  Push the
overlay onto `scf-invisible-overlays'."
  (let ((overlay (make-overlay start end)))
    (push overlay scf-invisible-overlays)
    (overlay-put overlay 'invisible 'scf)))

(defun scf-unhide ()
  "Show all areas hidden "
  (dolist (overlay scf-invisible-overlays)
    (delete-overlay overlay))
  (setq scf-invisible-overlays nil))

(defun scf-has-face (face &optional pos)
  "return true if POS has face."
  (setq pos (or pos (point)))
  (let ((faces (get-text-property (point) 'face)))
    (setq faces (if (symbolp faces)
                    (list (symbol-name faces))
                  (mapcar 'symbol-name faces)))
    (dolist (f faces)
      (when (string-match (format "\\`%s" face) f)
        (return t)))))


(define-minor-mode scf-mode
  "shorten file names in a compilation buffer"
  nil
  " SCF"
  nil
  (if scf-mode
      (progn
        (unless (scf-mode-is-compilation major-mode)
          (error "only compilation derived modes need apply."))
        ;; if we are turning on the mode from a major-mode hook, then we need
        ;; to rerun ourselves after compilation finishes, as there is likely
        ;; no output yet.
        (add-hook 'compilation-finish-functions (lambda (buf msg) (set-buffer buf) (scf-mode 1)) nil t)
        (save-excursion
          (font-lock-fontify-region (point-min) (point-max))
          (goto-char (point-min))
          (while (not (eobp))
            (if (scf-has-face 'compilation-info)
                (let* ((start (point))
                       (fn (buffer-substring-no-properties
                            start
                            (loop until (progn
                                          (goto-char (or (next-single-property-change (point) 'face)
                                                         (point-max)))
                                          (not (scf-has-face 'compilation-info)))
                                  finally return (point))))
                       (base-name (file-name-nondirectory fn)))
                  (scf-add-invisible-overlay start (- (+ start (length fn)) (length base-name))))
              (goto-char (or (next-single-property-change (point) 'face)
                             (point-max)))))))
    (show-all-invisible)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scf-mode.el ends here
;;; find hide-lines from the emacswiki
