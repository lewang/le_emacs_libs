;;; TODO selecting all lines creats too many spurious spaces at EOL, maybe
;;; just the indentation?

(require 'multiple-cursors-core)

(defun le::fix-indentation-min-indent-in-region (beg end)
  "Return minimum-indent in region (COLUMN . POINT).

Blank lines are skipped."
  (goto-char beg)
  (let ((min-indent (cons 9999 (point-min))))
    (while (< (point) end)
      (back-to-indentation)
      (unless (looking-at "\\s-*\n")
        (if (< (current-column) (car min-indent))
            (setq  min-indent (cons (current-column)
                                    (point)))))
      (forward-line 1))
    min-indent))

(defun le::fix-indentation (beg end)
  "Add cursors where appropriate to fix indenttion."
  (interactive "*r")
  (deactivate-mark)
  (let ((min-col (car (le::fix-indentation-min-indent-in-region beg end)))
        first-cursor)
    (goto-char beg)
    (while (< (point) end)
      (back-to-indentation)
      (unless (looking-at "\\s-*\n")
        (move-to-column min-col t)
        (if first-cursor
            (mc/create-fake-cursor-at-point)
          ;; Don't create the first cursor now; it gets created when
          ;; the mode is activated.
          (setq first-cursor (point))))
      (forward-line 1))
    (when first-cursor
      (goto-char first-cursor)
      (mc/maybe-multiple-cursors-mode))))

;; (global-set-key (kbd "<C-S-return>") 'le::fix-indentation)