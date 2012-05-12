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
;; Last-Updated: Sun May 13 00:45:51 2012 (+0800)
;;           By: Le Wang
;;     Update #: 29
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
  (concat
   (when (not (zerop (length stdout)))
     (concat
      "\t;;; ⇒ <STDOUT>\n"
      "\t;;; "
      (replace-regexp-in-string
       "\n"
       "\n\t;;; "
       stdout)
      "\n"))
   "\t;;; ⇒ "
   (replace-regexp-in-string
    "\n"
    "\n\t;;; "
    res)
   "\n"))

;;;###autoload
(defun le::eval-and-insert-results (beg end)
  "eval forms in region and insert results in a line underneath each.

With universal prefix, clear results.

Without active region, use the whole buffer.

Calling repeatedly should update results."

  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (setq end (copy-marker end))
  (catch 'slime-error
    (save-excursion
      (goto-char beg)
      (do ()
          ((> (point) end))
        (forward-sexp 1)
        (when (not (> (point) end))
          (let* ((sexp-str (buffer-substring-no-properties beg (point)))
                 (result (if (consp current-prefix-arg)
                             ""
                           (case major-mode
                             ((clojure-mode)
                              (condition-case err
                                  (let ((res (slime-eval `(swank:eval-and-grab-output ,sexp-str))))
                                    (le::eair::format-eval-outpt (second res) (first res)))
                                (error
                                 (message "slime error encountered %s" err)
                                 (throw 'slime-error nil))))
                             (t
                              (le::eair::format-eval-outpt (prin1-to-string
                                                            (eval (read sexp-str))))))))
                 (result-length (length result))
                 (tab-space (make-string tab-width ? )))
            (forward-line 1)
            (if (and (eobp)
                     (not (bolp)))                    ; handle eob
                (insert "\n")
              (when (looking-at (concat "\\(?:\t\\|"
                                        tab-space
                                        "\\);;; ⇒.*\n?\\(?:\\(\t\\|"
                                        tab-space
                                        "\\);;; .*\n\\)*"))
                (delete-region (point) (match-end 0))))
            (insert result))
          (setq beg (point)))
        ;; skip over all comments
        (while (not (eq (point) (progn
                                  (comment-forward 1)
                                  (point)))))
        ;; if we are at EOF then there we've already evaluated the last
        ;; meaningful sexp.
        (when (eobp)
          (return))))))
	;;; ⇒ le::eval-and-insert-results




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; le-eval-and-insert-results.el ends here
