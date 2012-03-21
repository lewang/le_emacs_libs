;;; repeat-macro-def.el --- make interactive funcitons repeatable by final key-chord

;; this file is not part of Emacs

;; Copyright (C) 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: make interactive funcitons repeatable by final key-chord
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Tue Mar 20 19:41:11 2012 (+0800)
;; Version: 0.1
;; Last-Updated: Wed Mar 21 08:02:36 2012 (+0800)
;;           By: Le Wang
;;     Update #: 3
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;;    (require 'repeat-macro-def)
;;    (defun-repeatable enlarge-window)
;;    (defun-repeatable enlarge-window-horizontally)
;;    (global-set-key [remap enlarge-window] 'enlarge-window-repeatable)
;;    (global-set-key [remap enlarge-window-horizontally] 'enlarge-window-horizontally-repeatable)
;;

;;; Commentary:

;; Default binding for `enlarge-window' is "C-x ^", calling
;;
;;   (defun-repeatable enlarge-window-horizontally)
;;
;; defines
;;
;; `enlarge-window-repeatable', which is repeatable by "^".
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A previous version of this function used `repeat', but setting          ;;
;; `last-repeatable-command' caused the help text to show "&rest --cl-end" ;;
;; instead of the formal arguments.  This is more functional as well.      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl))

(provide 'repeat-macro-def)

(defmacro defun-repeatable-int (name args &rest body)
  "Define interactive function that can be repeated by pressing last key cord."
  (declare (indent defun))
  (let* ((res body)
         (doc-string (when (stringp (car res))
                       (list (pop res))))
         (interactive-form (if (eq 'interactive (caar res))
                               (pop res)
                             (error "definition is not interactive"))))
    `(defun ,name ,args
       ,@doc-string
       ,interactive-form
       (let* ((repeat-key (and (> (length (this-single-command-keys)) 1)
                               last-input-event))
              (repeat-key-str (format-kbd-macro (vector repeat-key) nil))
              event)
         (loop do ,(cons 'progn res)
               while (progn
                       (message "(Type %s to repeat)" repeat-key-str)
                       (setq event (read-event))
                       (clear-this-command-keys t)
                       (if (equal event repeat-key)
                           (progn
                             (setq last-input-event nil)
                             t)
                         (push last-input-event unread-command-events)
                         nil)))))))


(defmacro defun-repeatable (func)
  (let ((new-func (intern (concat (symbol-name func) "-repeatable"))))
     (progn
     `(defun-repeatable-int ,new-func ()
        ,(format "repeatable version of `%s'" func)
        (interactive)
        (call-interactively (quote ,func))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; repeat-macro-def.el ends here

(provide 'repeat-macro-def)
