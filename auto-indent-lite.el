;;; auto-indent-lite.el --- auto indent when yank/yank-pop/kill-line
;;
;; Filename: auto-indent-lite-mode.el
;; Description: Auto Indent text on Yank/Paste
;; Author: Matthew L. Fidler & Others
;; Maintainer: Le Wang
;; Created: Sat Nov 6 11:02:07 2010 (-0500)
;; Version: 0.3
;; Last-Updated: Sat Mar 12 16:02:54 2011 (+0800)
;;
;; 21:13:09 2011 (+0800)
;;           By: Le Wang
;;     Update #: 497
;; URL: Keywords: Auto Indentation Compatibility: Tested with Emacs 23.2.1
;;
;; Features that might be required by this library:
;;
;;   None
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is auto-indent-mode without the minor-mode, and any hook setup.
;;
;; Only smart yank/yank-pop, and kill-line is left.
;;
;; This is completely *INCOMPATIBLE* with auto-indent-mode.el, if you need to
;; ensure correct indention at all times, I strongly recomment
;; auto-indent-mode.el.
;;
;; Installation:
;;
;;   (require 'auto-indent-light) (setq auto-indent-mode t)
;;
;; Customizaing:
;;
;; M-x customize-group, "auto-indent"

;;; Commentary (from auto-indent-mode.el):

;;  Provides auto-indentation minor mode.  This allows the following:
;;
;;  (1) Return automatically indents the code appropriately (if enabled)
;;
;;  (2) Pasting/Yanking indents the appropriately
;;
;;  (3) Killing line will take off unneeded spaces (if enabled)
;;
;;  (4) On visit file, indent appropriately, but DONT SAVE. (Pretend like
;;  nothing happened, if enabled)
;;
;;  (5) On save, optionally unttabify, remove trailing white-spaces, and
;;  definitely indent the file (if enabled).
;;
;;  (6) TextMate behavior of keys if desired (see below)
;;
;;  All of these options can be customized. (customize auto-indent)
;;
;;  To use put this in your load path and then put the following in your emacs
;;  file:
;;
;;  (setq auto-indent-on-visit-file t) ;; If you want auto-indent on for files
;;  (require 'auto-indent-mode)
;;
;;  If you (almost) always want this on, add the following to ~/.emacs:
;;
;;  (auto-indent-global-mode)
;;
;;  Excluded modes are defined in `auto-indent-disabled-modes-list'
;;
;;  If you only want this on for a single mode, you would add the following to
;;  ~/.emacs
;;
;;  (add-hook 'emacs-lisp-mode-hook 'auto-indent-minor-mode)
;;
;;  You could always turn on the minor mode with the command
;;  `auto-indent-minor-mode'
;;
;;  If you would like TextMate behavior of Meta-RETURN going to the end of the
;;  line and then inserting a newline, as well as Meta-shift return going to
;;  the end of the line, inserting a semi-colon then inserting a newline, use
;;  the following:
;;
;;
;;  (setq auto-indent-key-for-end-of-line-then-newline "<M-return>") (setq
;;  auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")
;;  (require 'auto-indent-mode) (auto-indent-global-mode)
;;
;;  This may or may not work on your system.  Many times emacs cannot
;;  distinguish between M-RET and M-S-RET, so if you don't mind a slight
;;  redefinition use:
;;
;;  (setq auto-indent-key-for-end-of-line-then-newline "<M-return>") (setq
;;  auto-indent-key-for-end-of-line-insert-char-then-newline "<C-M-return>")
;;  (require 'auto-indent-mode) (auto-indent-global-mode)
;;
;;
;;  If you want to insert something other than a semi-colon (like a colon) in
;;  a specific mode, say colon-mode, do the following:
;;
;;  (add-hook 'colon-mode-hook (lambda () (setq auto-indent-eol-char ":")))
;;
;;
;;  If you wish to use this with autopairs and yasnippet, please load this
;;  library first.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 31-Jan-2011 Matthew L. Fidler
;;    Last-Updated: Mon Jan 31 22:05:59 2011 (-0600) #440 (Matthew L. Fidler)
;;    Removed indirect reference to `shrink-whitespaces'.  Thanks Le Wang
;; 31-Jan-2011 Matthew L. Fidler
;;    Last-Updated: Mon Jan 31 21:27:39 2011 (-0600) #435 (Matthew L. Fidler)
;;    Added explicit requirement for functions
;; 18-Jan-2011 Matthew L. Fidler
;;    Last-Updated: Tue Jan 18 10:23:43 2011 (-0600) #428 (Matthew L. Fidler)
;;    Added support to turn on `org-indent-mode' when inside an org-file.
;; 12-Jan-2011 Matthew L. Fidler
;;    Last-Updated: Wed Jan 12 16:27:21 2011 (-0600) #420 (Matthew L. Fidler)
;;    Added fix for ortbl-minor-mode.  Now it will work when orgtbl-minor mode
;;    is enabled.
;; 09-Dec-2010 Matthew L. Fidler
;;    Last-Updated: Thu Dec 9 09:17:45 2010 (-0600) #414 (Matthew L. Fidler)
;;    Bugfix.  Now instead of indenting the region pasted, indent the
;;    region-pasted + beginning of line at region begin and end of line at
;;    region end.
;; 02-Dec-2010 Matthew L. Fidler
;;    Last-Updated: Thu Dec 2 13:02:02 2010 (-0600) #411 (Matthew L. Fidler)
;;    Made ignoring of modes with indent-relative and indent-relative-maybe
;;    apply to indenting returns as well.
;; 02-Dec-2010 Matthew L. Fidler
;;    Last-Updated: Thu Dec 2 11:38:37 2010 (-0600) #402 (Matthew L. Fidler)
;;    Removed auto-indent on paste/yank for modes with indent-relative and
;;    indent-relative-maybe.  This has annoyed me forever.
;; 02-Dec-2010 Matthew L. Fidler
;;    Last-Updated: Thu Dec 2 10:40:05 2010 (-0600) #397 (Matthew L. Fidler)
;;    Added an advice to delete-char.  When deleting a new-line character,
;;    shrink white-spaces afterward.
;; 02-Dec-2010 Matthew L. Fidler
;;    Last-Updated: Thu Dec 2 08:59:49 2010 (-0600) #386 (Matthew L. Fidler)
;;    Speed enhancement by checking for yasnippets only on indentation.
;; 29-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Mon Nov 29 13:19:38 2010 (-0600) #377 (Matthew L. Fidler)
;;    Bug fix to allow authotkey files to save.
;; 29-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Mon Nov 29 12:10:09 2010 (-0600) #367 (Matthew L. Fidler)
;;    Change auto-indent-on-save to be disabled by default.
;; 22-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Mon Nov 22 14:36:10 2010 (-0600) #365 (Matthew L. Fidler)
;;    Yasnippet bug-fix.
;; 22-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Mon Nov 22 12:00:07 2010 (-0600) #363 (Matthew L. Fidler)
;;    auto-indent bug fix for save on save buffer hooks.
;; 16-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Tue Nov 16 13:16:05 2010 (-0600) #361 (Matthew L. Fidler)
;;    Added conf-windows-mode to ignored modes.
;; 15-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Mon Nov 15 17:23:03 2010 (-0600) #354 (Matthew L. Fidler)
;;    Bugfix for deletion of whitespace
;; 15-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Mon Nov 15 14:27:50 2010 (-0600) #351 (Matthew L. Fidler)
;;    Bugfix for post-command-hook.
;; 15-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Mon Nov 15 08:53:03 2010 (-0600) #338 (Matthew L. Fidler)
;;    Added diff-mode to excluded modes for auto-indentaion.
;; 15-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Mon Nov 15 00:22:30 2010 (-0600) #336 (Matthew L. Fidler)
;;    Added fundamental mode to excluded modes for auto-indentation.
;; 13-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Sat Nov 13 20:03:10 2010 (-0600) #334 (Matthew L. Fidler)
;;    Bug fix try #3
;; 13-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Sat Nov 13 19:55:29 2010 (-0600) #329 (Matthew L. Fidler)
;;    Anothe bug-fix for yasnippet.
;; 13-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Sat Nov 13 19:49:47 2010 (-0600) #325 (Matthew L. Fidler)
;;
;;    Bug fix for auto-indent-mode.  Now it checks to make sure that
;;    `last-command-event' is non-nil.
;;
;; 11-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Thu Nov 11 13:56:15 2010 (-0600) #308 (Matthew L. Fidler)
;;    Put back processes in.  Made the return key handled by pre and
;;    post-command-hooks.
;; 11-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Thu Nov 11 11:28:42 2010 (-0600) #257 (Matthew L. Fidler)
;;    Took out processes such as *R* or *eshell*
;; 09-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Tue Nov 9 22:03:34 2010 (-0600) #255 (Matthew L. Fidler)
;;
;;    Bug fix when interacting with the SVN version of yasnippet.  It will not
;;    perform the line indentation when Yasnippet is running.
;;
;; 09-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Tue Nov 9 13:47:18 2010 (-0600) #253 (Matthew L. Fidler)
;;    Made sure that the auto-paste indentation doesn't work in minibuffer.
;; 09-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Tue Nov 9 11:51:07 2010 (-0600) #246 (Matthew L. Fidler)
;;    When `auto-indent-pre-command-hook' is inactivated by some means, add it
;;    back.
;; 09-Nov-2010
;;    Last-Updated: Tue Nov 9 11:13:09 2010 (-0600) #238 (Matthew L. Fidler)
;;    Added snippet-mode to excluded modes.  Also turned off the kill-line by
;;    default.
;; 07-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Sun Nov 7 18:24:05 2010 (-0600) #233 (Matthew L. Fidler)
;;    Added the possibility of TextMate type returns.
;; 07-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Sun Nov 7 00:54:07 2010 (-0500) #180 (Matthew L. Fidler)
;;    Bug fix where backspace on indented region stopped working.Added
;;    TextMate
;; 07-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Sun Nov 7 00:30:54 2010 (-0500) #167 (Matthew L. Fidler)
;;    Another small bug fix.
;; 07-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Sun Nov 7 00:21:38 2010 (-0500) #154 (Matthew L. Fidler)
;;
;;    Added bugfix and also allow movement on blank lines to be automatically
;;    indented to the correct position.
;;
;; 06-Nov-2010 Matthew L. Fidler
;;    Last-Updated: Sat Nov 6 17:39:59 2010 (-0500) #113 (Matthew L. Fidler)
;;    Initial release.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup auto-indent nil
  "* Auto Indent Mode Customizations"
  :group 'editing)

(defcustom auto-indent-mode nil
  "when t, adviced functions are active, else not active."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-mode-untabify-on-yank-or-paste 't
  "* Untabify pasted or yanked region."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-delete-line-char-remove-extra-spaces t
  "* When deleting a return, delete any extra spaces between the newly joined lines"
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-kill-line-remove-extra-spaces t
  "* When killing lines, remove extra spaces before killing the line."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-kill-line-at-eol 'subsequent-whole-line
  "* When killing lines, if at the end of a line,

nil - join next line to the current line. Deletes whitespace at
      join.  [this essentially duplicated delete-char]
      See also `auto-indent-kill-line-remove-extra-spaces'
whole-lines - kill next lines
blanks - kill all empty lines after the current line, and then
         any lines specified.

You should also set `kill-whole-line' to do what you want.

"
  :type '(choice (const :tag "default" nil)
                 (const :tag "next whole line" whole-line)
                 (const :tag "merge lines on first call, subsequent kill whole lines" subsequent-whole-line)
                 (const :tag "next whole line after any blank lines" blanks))
  :group 'auto-indent)

(defcustom auto-indent-kill-line-kill-region-when-active t
  "* When killing lines, if region is active, kill region instead."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-use-text-boundaries nil
  "* When killing lines, if point is before any text, act as if
  point is at BOL.  And if point is after text, act as if point
  is at EOL"
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-disabled-modes-list
  '(eshell-mode
    wl-summary-mode
    compilation-mode
    org-mode
    text-mode
    dired-mode
    snippet-mode
    fundamental-mode
    diff-mode
    texinfo-mode
    conf-windows-mode
    yaml-mode)
  "* List of modes disabled when auto-indent-lite is on."
  :type '(repeat (sexp :tag "Major mode"))
  :group 'auto-indent)

(defadvice yank (after auto-indent-mode-advice activate)
  (when (and auto-indent-mode
             (or (called-interactively-p 'any)
                 (memq this-command (list (key-binding [(control y)]) 'yank)))
             (not (memq major-mode auto-indent-disabled-modes-list))
             (not current-prefix-arg)
             (not (minibufferp)))
    (let ((mark-even-if-inactive transient-mark-mode))
      (when auto-indent-use-text-boundaries
        (let ((orig-m (point-marker)))
          (goto-char (mark))
          (backward-delete-char (skip-chars-forward " \t"))
          (goto-char orig-m)))
      (indent-region (region-beginning) (region-end))
      (indent-according-to-mode)
      (if auto-indent-mode-untabify-on-yank-or-paste
          (untabify (region-beginning) (region-end))))))

(defadvice yank-pop (after auto-indent-mode-advice activate)
  (when (and auto-indent-mode
             (or (called-interactively-p 'any)
                 (memq this-command (list (key-binding [(meta y)]) 'yank)))
             (not (memq major-mode auto-indent-disabled-modes-list))
             (not current-prefix-arg)
             (not (minibufferp)))
    (let ((mark-even-if-inactive transient-mark-mode))
      (when auto-indent-use-text-boundaries
        (let ((orig-m (point-marker)))
          (goto-char (mark))
          (backward-delete-char (skip-chars-forward " \t"))
          (goto-char orig-m)))
      (indent-region (region-beginning) (region-end))
      (indent-according-to-mode)
      (if auto-indent-mode-untabify-on-yank-or-paste
          (untabify (region-beginning) (region-end))))))

(defadvice delete-char (around auto-indent-mode activate)
  "If at the end of the line, take out whitespace after deleting character

This advice only works when you press the key mapped to
delete-char.  The behaviour of delete-char shouldn't be changed
in non-interactive calls.
"
  (if (or (called-interactively-p 'any)
          (eq (key-binding [delete]) this-command))
      (let ((del-eol (eolp)))
        ad-do-it
        (when (and del-eol
                   auto-indent-mode
                   (not (minibufferp))
                   auto-indent-delete-line-char-remove-extra-spaces)
          (fixup-whitespace)
          (when (and (eolp) (looking-back "[ \t]+" nil t))
            (replace-match ""))))
    ad-do-it))

(defadvice kill-line (around auto-indent-mode activate)
  "Obey `auto-indent-use-text-boundaries'.

Kill region if region is active

Kill backwards if UNIVERSAL ARG C-u is passed.

If at end of line, obey `auto-indent-kill-line-at-eol'

Consecutive kill-lines cause lines to be appended to last kill.

"
  (if (and auto-indent-mode
             (not (minibufferp))
             (or (called-interactively-p 'any)
                 (memq this-command (list (key-binding [(control k)])))))
      (let ((real-this-command this-command)
            ;; we don't want any functions we call to change
            (this-command this-command)
            ;; may be modified by (apend-next-kill)
            (last-command last-command)
            (ad-get-arg-0-int (prefix-numeric-value (ad-get-arg 0))))
        (when (memq real-this-command (list last-command 'kill-line))
          ;; this may change this-command to 'kill-region
          (append-next-kill))

        (cond ((and auto-indent-kill-line-kill-region-when-active
                    (use-region-p))
               (kill-region (region-beginning) (region-end)))
              ((and (ad-get-arg 0) (listp (ad-get-arg 0)))
               (if (auto-indent-bolp)
                   (ad-set-arg 0 -1)
                 (ad-set-arg 0 0))
               ad-do-it)
              ((auto-indent-bolp)
               (move-beginning-of-line 1)
               ad-do-it)
              ((auto-indent-eolp)
               (cond ((eq auto-indent-kill-line-at-eol nil)
                      (if (= ad-get-arg-0-int 1)
                          (delete-indentation 't)
                        ad-do-it))
                     ((eq auto-indent-kill-line-at-eol 'subsequent-whole-line)
                      (let (auto-indent-kill-line-at-eol)
                        (if (memq last-command '(kill-region real-this-command))
                            (progn
                              (setq auto-indent-kill-line-at-eol 'whole-line)
                              (kill-line (ad-get-arg 0)))
                          (setq auto-indent-kill-line-at-eol nil)
                          (kill-line (ad-get-arg 0)))))
                     ((memq auto-indent-kill-line-at-eol '(whole-line blanks))
                      (if (> (prefix-numeric-value (ad-get-arg 0)) 0)
                          (progn
                            (delete-region (point) (point-at-eol))
                            (unless (eobp)
                              (kill-region (point)
                                           (if (eq auto-indent-kill-line-at-eol 'blanks)
                                               (+ (point)
                                                  (skip-chars-forward " \t\n")
                                                  (skip-chars-backward " \t")
                                                  ;;
                                                  -1)
                                             (point-at-eol (1+ ad-get-arg-0-int))))))))
                     (t
                      (error "invalid auto-indent-kill-line-at-eol setting %s"
                             auto-indent-kill-line-at-eol))))
              (t
               ad-do-it))
        (when (not (memq major-mode auto-indent-disabled-modes-list))
          (indent-according-to-mode)))
    ad-do-it))

(defun auto-indent-eolp ()
  "returns t if point is at eol respecting `auto-indent-use-text-boundaries'"
  (if auto-indent-use-text-boundaries
      (looking-at-p "[ \t]*$")
    (eolp)))

(defun auto-indent-bolp ()
  "returns t if point is at bol respecting `auto-indent-use-text-boundaries'"
  (if auto-indent-use-text-boundaries
      (looking-back "^[ \t]*")
    (bolp)))

(provide 'auto-indent-lite)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-indent-lite.el ends here
