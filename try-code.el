;;; try-code.el --- try out a new idea by commenting out old code

;; Filename: try-code.el Description: Author: Le Wang Maintainer: Le Wang\
;; Created: Wed Feb  2 23:09:17 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Thu Feb  3 17:03:23 2011 (+0800)
;;           By: Le Wang
;;     Update #: 17
;; URL: https://github.com/lewang/le_emacs_libs/blob/master/try-code.el
;; Keywords: programming language modes
;; Compatibility:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; installation:

;;           (autoload 'try-code "try-code"  nil t)
;;           (global-set-key [(control \?)] 'try-code)

;;; Commentary:

;; Comments out a chunk of code to allow you to try some new code.  When you
;; reinvoke the function in the old or new section, you can choose which one
;; to keep.

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


(defgroup try-code nil
  "easily try a new piece of code"
  :group 'editing)

(defface try-code-old-code-face
  '((t :background "red4" :foreground "yellow"))
  ""
  :group 'try-code)

(defface try-code-test-code-face
  '((t :background "cyan" :foreground "grey10"))
  ""
  :group 'try-code)

(defvar try-code-whitespace-string
  " \t\v\n\r")

(defvar try-code-fill-length
  35)

(defvar try-code-start-prefix "<<<<<<< ")

(defvar try-code-middle-prefix "======= ")

(defvar try-code-end-prefix ">>>>>>>")

(defvar try-code-start-string
  (concat
   try-code-start-prefix
   user-login-name
   " "))

(defvar try-code-middle-string
  (concat
   try-code-middle-prefix
   " *test code* "))


(defvar try-code-end-string
  (concat
   try-code-end-prefix
   " *end* "))

(defun try-code-fill-string (text fill-string-atom length &optional left-justified)
  "fills `text' upto `length' with `fill-string-atom', right justified.

note `left-justified' option.

If \(length text\) > length, truncation is performed, `left-justified' is
still respected."
  (if (> (length text) length)
      (if left-justified
          (substring text 0 length)
        (substring text (- length)))
    (let ((fill-string-length (- length
                                 (length text)))
          (fill-string-atom-length (length fill-string-atom))
          fill-string)
      (when (< fill-string-atom-length 1)
        (error "`fill-string-atom' can't be null."))
      (setq fill-string
            (mapconcat 'identity
                       (make-vector (ceiling (/
                                              (float fill-string-length)
                                              fill-string-atom-length))
                                    fill-string-atom)
                       ""))
      (when (> (length fill-string) fill-string-length)
        (setq fill-string (substring
                           fill-string
                           0
                           fill-string-length)))
      (if left-justified
          (concat text fill-string)
        (concat fill-string text)))))

(setq try-code-start-string
      (concat
       try-code-start-string
       (try-code-fill-string
        ": *old code* " ">" (- try-code-fill-length (length try-code-start-string)) t)))

(setq try-code-middle-string
      (concat
       try-code-middle-string
       (try-code-fill-string
         nil "=" (- try-code-fill-length (length try-code-middle-string)) t)))

(setq try-code-end-string
      (concat
       try-code-end-string
       (try-code-fill-string
         nil "<" (- try-code-fill-length (length try-code-end-string)) t)))

(defun try-code-get-char (prompt accept-string)
  "repeatedly read chars until getting one in accept-string.
  char inputs are case insensitive.
  accept-string is case insensitive."
  (let ((cont t)
        (accept-list (string-to-list (upcase accept-string)))
        char)
    (while cont
      (message prompt)
      (setq char (read-char prompt))
      (when (memq (upcase char) accept-list)
        (setq cont nil)))
    char))

(defun try-code-region-is-a-comment (beg end)
  "returns t if region is a comment, else nil

"
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; This function is broken in 3 ways:           ;;
   ;;                                              ;;
   ;; 1. Quoted comment start.                     ;;
   ;;                                              ;;
   ;; 2. Multiline comment formats like Java or C: ;;
   ;;                                              ;;
   ;; /*                                           ;;
   ;;  * comment                                   ;;
   ;;  */                                          ;;
   ;;                                              ;;
   ;; 3. comment before code => THIS IS FIXED.     ;;
   ;;                                              ;;
   ;; /* comment */ int /* comment 2 */ a = 5;     ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (save-excursion
    (let ((continue t)
          result
          comment-found
          movement
          temp)
      ;; try to move to proper starting position
      (setq temp (min beg end))
      (setq end (max beg end))
      (setq beg temp)
      (setq temp nil)
      (goto-char beg)
      (forward-line 0)

      ;; by pass comments on the same line before beg
      (while continue
        (if (and comment-start
                 (re-search-forward (regexp-quote comment-start)
                                      (point-at-eol) t)
                 (<= (match-beginning 0) beg))
            (progn
              (goto-char (match-beginning 0))
              (setq temp (point))
              (if (and comment-end
                       (not (= 0 (length comment-end)))
                       (re-search-forward (regexp-quote comment-end)
                                          (point-at-eol) t)
                       (< (match-end 0) beg))
                  (goto-char (match-end 0))
                (setq continue nil)))
          (setq continue nil)))

      (and temp
           (goto-char temp))

      (setq temp nil)
      (setq continue t)

      (while continue
        (setq movement (forward-comment 1))
        (if movement
            (setq comment-found t)
          (setq movement (not (= (skip-chars-forward try-code-whitespace-string)
                                 0))))
        (if movement
            (when (>= (point) end)
              (setq result   t
                    continue nil))
          (setq continue nil)))
      (setq result (and result comment-found))
      (if (interactive-p)
          (message "region is %s comment."
                   (if result
                       "a"
                     "not a")))
      result)))

(defun try-code-parse-line (&optional point)
  "return various information about the line at point in an alist.

text-begin-pos starts after spaces and read-only text"
  (setq point (or point
                  (point)))
  (save-excursion
    (goto-char point)
    (beginning-of-line 1)
    (let ((comment-begin-pos (when (and comment-start-skip
                                        (re-search-forward
                                         comment-start-skip
                                         (point-at-eol)
                                         t))
                               (prog1
                                   (point)
                                 (beginning-of-line 1))))
          (text-begin-pos (let ((indent-tabs-mode nil)
                                pos)
                            (setq pos (cond ((get-text-property (point) 'read-only)
                                             (next-single-property-change
                                              (point) 'read-only nil (point-at-eol)))
                                            ((featurep 'filladapt)
                                             (+ (length (filladapt-make-fill-prefix
                                                         (filladapt-parse-prefixes)))
                                                (point-at-bol)))
                                            (t
                                             (+ (length (fill-context-prefix
                                                         (point-at-bol)
                                                         (point-at-eol)))
                                                (point-at-bol)))))
                            (beginning-of-line 1)
                            pos))

          (text-end-pos (prog2
                            (re-search-forward
                             "[ \t]*$"
                             (point-at-eol)
                             t)
                            (match-beginning 0)
                          (beginning-of-line 1))))
      (list (cons 'comment-begin-pos comment-begin-pos)
            (cons 'text-begin-pos text-begin-pos)
            (cons 'text-end-pos text-end-pos)
            (cons 'is-comment-p (and comment-begin-pos
                                     (= text-begin-pos
                                        comment-begin-pos)))
            ))))


(defun try-code-region-is-whitespace (beg end)
  (save-excursion
    (goto-char beg)
    (skip-chars-forward try-code-whitespace-string end)
    (if (< (point) end)
        nil
      t)))

(defun try-code-get-comment-info-alist (point)
  "Return info about the last old/test code pair in an alist,
or nil if none found.

Searching starts from point backwards, "
  (save-excursion
    (goto-char point)
    (let (code-region-start
          old-code-start
          old-code-end
          test-code-start
          test-code-end
          code-region-end
          code-region-line-count
          old-code-line-count
          test-code-line-count
          alist)
      (when
          (and
           point
           (goto-char (point-at-eol))
           (search-backward try-code-start-prefix nil t)
           ;; `forward-comment' behaves unexpectedly unless used at bol.
           (setq code-region-start (point-at-bol))
           (setq old-code-start (point-at-bol 2))
           (search-forward try-code-middle-prefix nil t)
           (setq old-code-end (point-at-bol))
           (setq  test-code-start (point-at-bol 2))
           (try-code-region-is-a-comment code-region-start test-code-start)
           ;; move past all code regions within the test-code-region
           (let (done
                 temp-code-end
                 temp-code-start
                 temp-alist)
             (while (not done)
               (if (and
                    (save-excursion
                      (setq temp-code-end
                            (search-forward
                             try-code-end-prefix nil t)))
                    (save-excursion
                      (setq temp-code-start
                            (search-forward
                             try-code-start-prefix nil t)))
                    (> temp-code-end temp-code-start))
                   (progn
                     (setq temp-alist
                           (try-code-get-comment-info-alist temp-code-start))
                     (if temp-alist
                         (goto-char (cdr (assoc 'code-region-end temp-alist)))
                       (progn
                         (goto-char temp-code-end)
                         (setq done t))))
                 (setq done t)))
             (and temp-code-end
                  (goto-char temp-code-end)
                  (setq test-code-end (point-at-bol))
                  (setq code-region-end (point-at-bol 2))))
           (try-code-region-is-a-comment test-code-end (point)))
          (list
            (cons 'code-region-start code-region-start)
            (cons 'code-region-end code-region-end)
            (cons 'code-region-line-count (count-lines code-region-start
                                                       code-region-end))
            (cons 'old-code-start old-code-start)
            (cons 'old-code-end old-code-end)
            (cons 'old-code-line-count (count-lines old-code-start
                                                    old-code-end))
            (cons 'test-code-start test-code-start)
            (cons 'test-code-end test-code-end)
            (cons 'test-code-line-count (count-lines test-code-start
                                                     test-code-end)))))))

(defun try-code-comment-make-old-test-region (beg end)
  "make a old/test region out of the parameter region

point is moved to appropriate insertion point."


  (if (and (try-code-region-is-a-comment beg end)
           (not (try-code-region-is-whitespace beg end)))
      (error "this function is meaningless for a comment region.")
    (let (diff
          end-m
          insertion-mark)
      (setq beg (min beg end))          ;set variables
      (setq end (max beg end))
      (setq diff (- end beg))
      (setq end-m (make-marker))
      (set-marker end-m end)
      (set-marker-insertion-type end-m t)
      (goto-char beg)                   ;break lines if necessary
      (unless (<= (point) (cdr (assq 'text-begin-pos (try-code-parse-line))))
        (insert "\n")
        (setq beg (point)))
      (goto-char end-m)
      ;; end-m will be at the beginning of a line
      (cond ((= (point-at-bol) (point))
             nil)
            ((= (point-at-eol) (point))
             (if (eobp)
                 (insert "\n")
               (forward-char 1)
               (set-marker end-m (1+ end-m))))
            (t
             (insert "\n")))
      (copy-region-as-kill beg end-m)   ;copy the region to be modified
      (goto-char beg)                   ;start modifying text
      (insert try-code-start-string "\n")
      (goto-char end-m)
      (insert try-code-middle-string "\n")
      (comment-region beg end-m)
      (setq insertion-mark (point))
      (insert "\n")
      (insert try-code-end-string "\n")
      (comment-region (1+ insertion-mark) (point))
      (goto-char insertion-mark)
      (message "Your old code is in the kill-ring.")
      (set-marker end-m nil)
      )))

(defun try-code-comment-unmake-old-test-region (point)
  "Unmake a old/test region around `point'.

User is offered a choice of which to keep."
  (let* ((properties (try-code-get-comment-info-alist point))
         (code-region-start (cdr (assoc 'code-region-start properties)))
         (code-region-end (cdr (assoc 'code-region-end properties)))
         (code-region-line-count (cdr (assoc 'code-region-line-count properties)))
         (old-code-start (cdr (assoc 'old-code-start properties)))
         (old-code-end (cdr (assoc 'old-code-end properties)))
         (old-code-line-count (cdr (assoc 'old-code-line-count properties)))
         (test-code-start (cdr (assoc 'test-code-start properties)))
         (test-code-end (cdr (assoc 'test-code-end properties)))
         (test-code-line-count (cdr (assoc 'test-code-line-count properties)))
         old-code-overlay
         test-code-overlay)
    (unless
        properties
      (error "try-code-comment-unmake-old-test-region: appropriate region not found."))
    (goto-char code-region-start)
    (condition-case err
        (let (done
              char
              selected-string)
          (setq old-code-overlay (make-overlay old-code-start old-code-end))
          (overlay-put old-code-overlay 'face 'try-code-old-code-face)
          (setq test-code-overlay (make-overlay test-code-start test-code-end))
          (overlay-put test-code-overlay 'face 'try-code-test-code-face)
          (setq char
                (try-code-get-char
                 (concat "Keep ("
                         (propertize "o" 'face 'try-code-old-code-face)
                         ")ld or ("
                         (propertize "t" 'face 'try-code-test-code-face)
                         ")test? ")
                 "ot"))
          (cond ((= ?o  char)
                 (goto-char old-code-start)
                 (comment-region (point)
                                 (point-at-bol (1+ old-code-line-count))
                                 '(4))
                 (setq selected-string
                       (buffer-substring-no-properties
                        old-code-start
                        (point-at-bol (1+ old-code-line-count)))))
                ((= ?t char)
                 (goto-char test-code-start)
                 (setq selected-string
                       (buffer-substring-no-properties
                        test-code-start
                        (point-at-bol (1+ test-code-line-count))))
                 (when (string-match  "\`[\n\r\t ]*\'" selected-string)
                   (setq selected-string ""))))
          (delete-overlay old-code-overlay)
          (delete-overlay test-code-overlay)
          (goto-char code-region-start)
          (delete-region code-region-start
                         (point-at-bol (1+ code-region-line-count)))
          (insert selected-string)
          (goto-char code-region-start))
      (error (and (overlayp old-code-overlay)
                  (delete-overlay old-code-overlay))
             (and (overlayp old-code-overlay)
                  (delete-overlay test-code-overlay))
             (signal (car err) (cdr err)))
      (quit (and (overlayp old-code-overlay)
                 (delete-overlay old-code-overlay))
            (and (overlayp old-code-overlay)
                 (delete-overlay test-code-overlay))
            (signal (car err) (cdr err))))))

;;;###autoload
(defun try-code ()

"comments out a piece of code and prepares to edit some experimental code.

At any time, this function can be called again to restore the old code or
implement the new code for good.

a dwim style is employed."
  (interactive "*")

  (let ((original-pos (point)))
    (condition-case err
        (if (use-region-p)
            (let ((beg (region-beginning))
                  (end (region-end)))
              (try-code-comment-make-old-test-region beg end))
          (try-code-comment-unmake-old-test-region (point)))
      (error (goto-char original-pos)
             (signal (car err) (cdr err)))
      (quit (goto-char original-pos)))))



(provide 'try-code)