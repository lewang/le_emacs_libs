;;; LRU-yank.el --- Least Recently Used stacking for kill-ring

;; Filename: LRU-yank.el
;; Description: Least Recently Used stacking for kill-ring
;; Author: Le Wang
;; Maintainer:  Le Wang (lewang.emacs!!!gmayo.com remove exclamations, correct host, hint: google mail)
;; Created: 2006/07/08 07:35:50
;;
;; Copyright (c) 2006, 2011 Le Wang

;; Version: 0.2
;; Last-Updated: Thu Feb  3 16:58:56 2011 (+0800)
;;           By: Le Wang
;;     Update #: 3
;; URL: https://github.com/lewang/le_emacs_libs/blob/master/LRU-yank.el
;; Keywords:
;; Compatibility: GNU Emacs 21, 23.2.1
;; Keywords: convenience editing
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Implements LRU (Least Recently Used) stacking for kill-ring.
;;
;; By default Emacs treats the kill-ring as a "ring", I find it more useful
;; to move each yanked item to the top of a stack instead.  This way
;; recently used (inserted) stuff are always at the top -- think alt-tab in
;; Windows.
;;
;; Before using this package, you should try M-- M-y (negative prefix arg to
;; yank-pop), as it may already do what you want.

       ;;,----
       ;;| ** When you use this package, realize that you are deciding to
       ;;|     prioritize "time of last use" over "time of introduction into
       ;;|     kill-ring".
       ;;| ** You actually lose the "time of addition into kill-ring"
       ;;|     information, because this library actively modifies the
       ;;|     kill-ring.
       ;;`----

;;; Installation:

;;   Put this file into your load-path and the following into your
;;   ~/.emacs:
;;
;;   (require 'LRU-yank)
;;   (setq LRU-yank-mode t)

;;; Related packages:

;;  * `buffer-stack'
;;  * `bubble'


;;; Code:

(provide 'LRU-yank)
(eval-when-compile
  (require 'cl))

(provide 'LRU-yank)

(defcustom LRU-yank-mode nil
  "*Non-nil means use use LRU order when yanking."
  :type 'boolean
  :group 'killing)

(defvar LRU-yank-count nil)
(defvar LRU-yank-count-prev nil)

(defun list-reorder (list i j)
  "move j_th elt of list to the i_th position, use i=0 for head.

return the new list."

  (let (i-after-list
        j-after-list
        j-element)
    (setq j-element (nthcdr j list)
          j-after-list (cdr j-element))

    (if (= j 0)
        (setq list (cdr list))
      (setcdr (nthcdr (- j 1) list) j-after-list))

    (setcdr j-element nil)

    ;; list ---- j-after-list --- nil

    (if (= i 0)
        (nconc j-element list)
      (setq i-after-list (nthcdr i list))
      (setcdr (nthcdr (- i 1) list) j-element)
      (nconc list i-after-list))))

(defadvice current-kill (around LRU-yank activate compile)
  "kill-ring stacking hook"
  (when (not (eq last-command 'yank))
    (setq LRU-yank-count 0)
    (setq LRU-yank-count-prev 0))

  (if (or (not LRU-yank-mode)
          (and (not (eq last-command 'yank))
               (or
                (and (= (ad-get-arg 0) 0)
                     interprogram-paste-function
                     (prog1
                         (funcall interprogram-paste-function)
                       ;; reset it so it can be accessed again
                       (setq x-last-selected-text nil)))
                (not kill-ring)      ; empty kill-ring
                )))
      ad-do-it
    (setq LRU-yank-count (+ LRU-yank-count (ad-get-arg 0)))
    (let ((n (mod LRU-yank-count
                  (length kill-ring)))
          (n-prev (mod LRU-yank-count-prev
                       (length kill-ring))))
      (when (not (= LRU-yank-count 0))
        (if (= n 0)
            (setq kill-ring (list-reorder kill-ring
                                          (- (length kill-ring)
                                             1)
                                          0))
          (setq kill-ring (list-reorder kill-ring n-prev 0)
                kill-ring (list-reorder kill-ring 0 n))))
      (setq kill-ring-yank-pointer kill-ring)
      (setq LRU-yank-count-prev LRU-yank-count)
      (setq ad-return-value (car kill-ring)))))



;;; LRU-yank.el ends here