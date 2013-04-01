;; -*- lexical-binding: t -*-
;;; le-eval-and-insert-results.el --- evaluates buffer and inline results

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: evaluates buffer and inline results
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Tue Sep 13 01:04:33 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Sat Mar 30 22:41:55 2013 (+0800)
;;           By: Le Wang
;;     Update #: 62
;; URL: https://github.com/lewang/le_emacs_libs/blob/master/le-eval-and-insert-results.el
;; Keywords: emacs-lisp evaluation
;; Compatibility: Emacs 23+

;;; Installation:

;;   (require 'le-eval-and-insert-results)
;;
;; M-x le::eval-and-insert-results
;;

;;; Commentary:

;; Simple function to evaluate buffer and inline results of each top-level
;; form as a comment.
;;
;; This is basically batch ielm.  I find it useful when experimenting with
;; stuff.
;;
;; Function is reentrant, and should update results each time.
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
	;;; ⇒ cl

(provide 'le-eval-and-insert-results)
	;;; ⇒ le-eval-and-insert-results

(defun le::eair::format-eval-outpt (res &optional stdout)
  (let ((triple-comment (make-string 3 (string-to-char comment-start))))
    (concat
     (when (not (zerop (length stdout)))
       (concat
        "\t"
        triple-comment
        " ⇒ <STDOUT>\n"
        "\t"
        triple-comment
        " "
        (replace-regexp-in-string
         "\n"
         (concat
          "\n\t"
          triple-comment
          " ")
         stdout)
        "\n"))
     "\t"
     triple-comment
     " ⇒ "
     (replace-regexp-in-string
      "\n"
      (concat
       "\n\t"
       triple-comment
       "   ")
      res)
     "\n")))

(defun le::eval-and-insert-all-sexps (beg end)
  "call `le::eval-and-insert-results' for all sexps in region.

With universal arguments, use whole buffer.
"
  (interactive (cond ((use-region-p)
                      (list (region-beginning) (region-end)))
                     ((consp current-prefix-arg)
                      (list (point-min) (point-max)))))
  (setq end (copy-marker end))
  (let ((forward-func (nth 1 (le::eair::get-movements))))
    (save-excursion
      (goto-char beg)
      (loop do (progn
                 (comment-forward 10000)
                 (funcall forward-func 1)
                 (call-interactively 'le::eval-and-insert-results))
            while (and (< (point) end)
                       (not (eobp))))))
  (set-marker end nil))

(defun le::eair::get-result-regexp ()
  "get regexp for current buffer"
  (let ((tab-space (make-string tab-width ? ))
        (triple-comment (make-string 3 (string-to-char comment-start))))
    (concat "\\(?:\t\\|"
            tab-space
            "\\)"
            triple-comment
            " ⇒.*\n?\\(?:\\(\t\\|"
            tab-space
            "\\)"
            triple-comment
            " .*\n\\)*")))

;;;###autoload
(defun le::clear-inserted-results (beg end)
  "clear inserted results in region."
  (interactive "*r")
  (save-excursion
    (let ((case-fold-search nil)
          (regexp (le::eair::get-result-regexp)))
      (setq end (copy-marker end t))
      (goto-char beg)
      (while (re-search-forward regexp end 'noerror)
        (replace-match ""))
      (set-marker end nil))))


(defvar le::eair::movements-alist
  '((clojure-mode          beginning-of-defun end-of-defun)
    (emacs-lisp-mode       beginning-of-defun end-of-defun)
    (lisp-interaction-mode beginning-of-defun end-of-defun)
    (t                     backward-paragraph forward-paragraph))
  "Per major-mode movements.")

(defun le::eair::get-movements ()
  "return list of movements for current major-mode"
  (let ((res (assq major-mode le::eair::movements-alist)))
    (cdr
     (or res
         (assq t le::eair::movements-alist)))))

(defun le::eair::result-handler-maker (pos)
  "create a function appropriate for handling result"
  (let ((marker (copy-marker pos)))
    (lambda (res &optional stdout)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (insert (le::eair::format-eval-outpt res stdout))))
      (set-marker marker nil))))


;;;###autoload
(defun le::eval-and-insert-results (beg end)
  "eval region as single form and append result to end."
  (interactive (save-excursion
                 (destructuring-bind (back-func forward-func)
                     (le::eair::get-movements)
                   (let ((initial-beg (progn
                                        (funcall back-func)
                                        (point)))
                         (initial-end (progn
                                        (funcall forward-func)
                                        (forward-comment 10000)
                                        (point))))
                     (le::clear-inserted-results initial-beg initial-end)
                     (goto-char initial-beg)
                     (skip-chars-forward " \t\n")
                     (list (point-at-bol 0)
                           (progn
                             (funcall forward-func)
                             (skip-chars-backward " \t\n")
                             (point)))))))
  (let* ((sexp-str (buffer-substring-no-properties beg end))
         (insert-at (save-excursion
                      (goto-char end)
                      (unless (bolp)
                        (if (eobp)
                            (insert "\n")
                          (forward-line 1)))
                      (point)))
         (handler (le::eair::result-handler-maker insert-at)))
    (case major-mode
      ('clojure-mode
       (let (res pretty-res)
         (setq res (slime-eval `(swank:eval-and-grab-output ,sexp-str)))
         (setq stdout-res (slime-eval `(swank:pprint-eval ,(concat "'" (second res)))))
         (funcall handler res stdout-res)))
      ((emacs-lisp-mode lisp-interaction-mode)                             ; emacs-lisps
       (funcall handler (prin1-to-string
                         (eval (read sexp-str)))))
      (t
       (require 'le-comint-gather-output)
       (le::comint-get-output sexp-str handler)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; le-eval-and-insert-results.el ends here
