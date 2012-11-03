;;; magitify.el --- magitify diff buffers

;; this file is not part of Emacs

;; Copyright (C) 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: magitify diff buffers
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Tue Sep 11 10:55:22 2012 (+0800)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 15
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;;
;;
;;

;;; Commentary:

;;
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

(require 'magit)

(unless (fboundp '__magit-diff-line-file)
  (fset '__magit-diff-line-file (symbol-function 'magit-diff-line-file)))

(defvar magitify-mode nil
  "Non-nil if Magitify mode is enabled.
Use the command `magitify-mode' to change this variable.")
(make-variable-buffer-local 'magitify-mode)

(defun magitify-mode (arg)
  "minor-mode for magitifying a diff buffer."
  (let ((last-message
        (current-message)))
    (setq magitify-mode
          (> (prefix-numeric-value arg) 0))
    (unless magitify-mode
      (diff-mode))
    (run-hooks 'magitify-mode-hook
               (if magitify-mode 'magitify-mode-on-hook 'magitify-mode-off-hook)))
  (force-mode-line-update)
  magitify-mode)

(defvar magitify-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-item] 'diff-goto-source)
    map)
  "Keymap for `magitify-mode'.")

(with-no-warnings
  (add-minor-mode 'magitify-mode '"magitify" magitify-mode-map nil nil))


(defun magitify-on ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^diff " nil t)
        (let ((case-fold-search nil))
          (forward-line 0)
          (flet ((erase-buffer ())
                 (magit-cmd-insert (cmd args) (goto-char (point-max)))
                 (magit-rev-range-describe (range things) ())
                 (magit-insert-diff-title (status file file2) ()))
            (magit-mode-init default-directory
                             'magit-diff-mode
                             #'magit-refresh-diff-buffer
                             nil nil))
          (magitify-mode 1))
      (message "no diff found."))))

;; (defun magitify-off ()
;;   (interactive)
;;   (magitify-mode -1))


;; (add-hook 'diff-mode-hook 'magitify-on)

;; (defadvice diff-sentinel (after magitify activate compile)
;;   "activate magitify"
;;   (magitify-on))

(provide 'magitify)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magitify.el ends here

