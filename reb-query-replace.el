;;; reb-query-replace.el --- query-replace re-builder 

;; this file is not part of Emacs

;; Copyright (C) 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description:
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Thu Jul  5 00:23:10 2012 (+0800)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 17
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


(provide 'reb-query-replace)

(require 're-builder)

(define-key reb-mode-map "\C-c\M-%" 'reb-query-replace-this-regxp)

(define-minor-mode reb-query-replace-mode
  "minor mode to facilitate `reb-query-replace'"
  nil
  " REB-REPLACE"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'reb-query-replace-doit)
    (define-key map (kbd "C-g") 'reb-quit)
    map)
  (if reb-query-replace-mode
      ;; select the existing regexp
      (let ((regexp (reb-read-regexp)))
        (setq header-line-format (substitute-command-keys "Hit \\[reb-query-replace-doit] to dance,  \\[reb-quite] to quit."))
        (when (not (zerop (length regexp)))
          (search-forward regexp)
          (goto-char (match-beginning 0))
          (push-mark (match-end 0) t t)
          (setq deactivate-mark nil)))
    (setq header-line-format nil)))

(defun reb-query-replace ()
  "Use re-builder to buildregexp, keystrokes should be
identical to `query-replace-regexp'."
  (interactive)
  (setq reb-re-syntax 'string)
  (re-builder)
  (reb-query-replace-mode 1))

(defun reb-query-replace-doit (replacement)
  "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.

Argument REPLACE String used to replacement the matched strings in the buffer.
 Subexpression references can be used (\1, \2, etc)."
  (interactive "sReplace with: ")
  (if (eq major-mode 'reb-mode)
      (let ((regexp (reb-read-regexp)))
        (reb-query-replace-mode -1)
        (reb-quit)
        (save-excursion
          (beginning-of-buffer)
          (query-replace-regexp regexp replacement)))
    (message "Not in a re-builder buffer!")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reb-query-replace.el ends here
