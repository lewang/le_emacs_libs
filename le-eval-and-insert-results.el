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
;; Last-Updated: Tue Sep 13 01:18:48 2011 (+0800)
;;           By: Le Wang
;;     Update #: 4
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

(provide 'le-eval-and-insert-results)

;;;###autoload
(defun le::eval-and-insert-results (beg end)
  "eval forms in region and insert results in a line underneath each.

Without active region, use the whole buffer.

Calling repeatedly should update results."

  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (save-excursion
    (goto-char beg)
    (while (<= (point) end)
      (forward-sexp 1)
      (when (<= (point) end)
        (let* ((result (concat
                        "\t;; ⇒ "
                        (replace-regexp-in-string
                         "\n"
                         "\n\t;; "
                         (prin1-to-string
                          (eval
                           (read
                            (buffer-substring-no-properties beg (point))))))
                        "\n"))
               (result-length (length result)))
          (goto-char (point-at-bol 2))
          (if (bolp)                    ; handle eob
              (when (looking-at "\t;; ⇒.*\n\\(?:\t;; .*\n\\)*")
                (delete-region (point) (match-end 0))
                (setq end (- end (- (match-end 0) (point)))))
            (insert "\n"))
          (insert result)
          (setq end (+ end result-length)))
        (setq beg (point))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; le-eval-and-insert-results.el ends here
