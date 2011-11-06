;;; autopair-padding.el --- add space padded pairs to autopair

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: pad autopair delimiters with
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Fri Sep 16 19:26:30 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Sun Oct 16 19:39:48 2011 (+0800)
;;           By: Le Wang
;;     Update #: 8
;;          URL: https://github.com/lewang/le_emacs_libs/blob/master/autopair-padding.el
;;
;; Keywords:
;; Compatibility: Emacs 23+

;;; Installation:

;;
;;    (add-hook 'ruby-mode-hook
;;          (lambda ()
;;            (require 'autopair-padding)
;;            (setq autopair-dont-pad '(:comment :string))
;;            (setq autopair-handle-action-fns
;;                  (list #'autopair-default-handle-action
;;                        #'autopair-padding-handler))
;;            (autopair-mode 1)))
;;

;;; Commentary:

;; Padding delimiters with a space is common in some languages for clarity,
;; this package integrates paddded delimiters into autopair.
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

(require 'autopair)

(provide 'autopair-padding)


(defvar autopair-dont-pad-contexts nil
  "list of contexts where padding should not occur
e.g. (setq autopair-dont-pad-contexts'(:comment :string))

see `autopair-extra-pairs' for valid keywords")

(defvar autopair-dont-pad nil
  "list of chars for which padding should not happen")

(defun autopair-padding-handler (action pair pos-before)
  "handler for space padded delimiters

When opening, insert \"( | )\" instead of \"()\".

When backspacing, deleting original opening delimiter deletes
entire delimited region if it's only one space.  So you can
backspace twice undo insertion.

When closing, if the delimited string unchanged, padding spaces
are removed.

Note: you should set `autopair-skip-whitespace' to skip the
closing padded delimiter properly."
  (let ((where-sym (second (autopair-syntax-ppss)))
        touched)
    (unless (or (eq :everywhere where-sym)
                (memq where-sym autopair-dont-pad-contexts)
                (memq pair autopair-dont-pad))
      (cond ((and (eq 'opening action)
                  (eq pair (char-after)))
             (insert "  ")
             (backward-char 1))
            ((and (eq 'backspace action)
                  (equal (format " %c" pair)
                         (buffer-substring-no-properties (point) (+ 2 (point)))))
             (delete-region (point) (+ 2 (point))))
            ((eq 'closing action)
             (let* ((open-delim pair)
                    (close-delim (autopair-calculate-inserted))
                    (match-regex (if autopair-skip-whitespace
                                     (format "%c\\(  \\)%c" open-delim close-delim)
                                   (format "%c\\( %c \\)%c" open-delim close-delim close-delim))))
               (save-excursion
                 (skip-chars-backward (format "^%c" open-delim) (point-at-bol))
                 (backward-char 1)
                 (when (looking-at match-regex)
                   (replace-match "" nil nil nil 1)
                   (setq touched t)))
               ;; move past close-delim
               (when (and touched
                          (not autopair-skip-whitespace))
                 (forward-char))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair-padding.el ends here
