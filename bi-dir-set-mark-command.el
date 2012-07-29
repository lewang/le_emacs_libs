;;; bi-dir-set-mark-command.el --- allow forward movement in mark-ring

;; this file is not part of Emacs

;; Copyright (C) 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description:allow forward movement in mark-ring
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Mon Jul 30 00:36:16 2012 (+0800)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 4
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;;
;; Add to your init file:
;;
;;    (require 'bi-dir-set-mark-command)
;;    (global-set-key [remap set-mark-command] 'bd/set-mark-command)
;;
;; if you cua-mode:
;;
;;    (cua-mode 1)
;;    (require 'bi-dir-set-mark-command)
;;    (define-key cua-global-keymap [remap set-mark-command] 'bd/set-mark-command)

;;; Commentary:

;;
;;  This is a rehash of two answers on SO:
;;
;;    http://stackoverflow.com/a/3399064/903943
;;    http://stackoverflow.com/a/5117076/903943
;;
;;  "C-- C-SPC" moves forward in mark-ring.  That is all.
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

(provide 'bi-dir-set-mark-command)

(defun unpop-to-mark-command ()
  "Unpop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (interactive)
  (setq this-command 'unpop-to-mark-command)
  (let ((num-times (if (equal last-command 'pop-to-mark-command)
                       2
                     1)))
    (dotimes (x num-times)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (+ 0 (car (last mark-ring))) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (mark t)))
      (deactivate-mark))))

;;;###autoload
(defun bd/set-mark-command (arg)
  "Enable reversing direction with un/pop-to-mark.

\"-\" goes forward, \"C-u\" goes backwards.

repeated press of C-SPC goes in same direction
"
  (interactive "P")
  (let (do-it)
    (cond
     ;; Enabled repeated un-pops with C-SPC
     ((eq last-command 'unpop-to-mark-command)
      (if (equal arg '(4))
          (progn                        ; C-u C-SPC reverses back to normal direction
            (pop-to-mark-command)       ; invoke twice to switch directions
            (setq do-it t))
        ;; Otherwise continue to un-pop
        (unpop-to-mark-command)))
     ;; Negative argument un-pops: C-- C-SPC
     ((eq arg '-)
      (unpop-to-mark-command))
     (t
      (setq do-it t)))
    (when do-it
      (let ((set-mark-cmd (if cua-mode
                              'cua-set-mark
                            'set-mark-command)))
        (setq this-command set-mark-cmd)
        (funcall set-mark-cmd arg)))))

