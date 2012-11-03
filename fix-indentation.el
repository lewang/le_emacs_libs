;;; TODO selecting all lines creats too many spurious spaces at EOL, maybe
;;; just the indentation?

(defun le::fix-indentation-extend (extend-to)
  (if iedit-rectangle
      (let ((beg (min (nth 0 iedit-rectangle) extend-to))
            (end (max (nth 1 iedit-rectangle) extend-to)))
        (iedit-rectangle-mode)
        (goto-char end)
        (skip-chars-backward " \t\n")

        (goto-char beg)
        (goto-char (point-at-bol))
        ;; skip spaces and lines
        (skip-chars-forward " \t\n")
        ;; include indentation in rectangle
        (goto-char (point-at-bol))
        (setq beg (point))
        (let ((min-indent (cons 9999 (point-min))))
          (while (< (point) end)
            (back-to-indentation)
            (if (< (current-column) (car min-indent))
                (setq  min-indent (cons (current-column)
                                        (point))))
            (forward-line 1))
          (goto-char end)
          (move-to-column (car min-indent) t)
          (iedit-rectangle-mode beg (point))
          (goto-char (cdr min-indent))))
    (error "no current rectangle")))


(defun le::fix-indentation (beg end)
  "fix indentation by editing expanded region as rectangle.

when rectangle is active:

  when point is outside rectangle, extend it to include point.
  when point is inside rectangle, cancel `iedit-rect-mode'"
  (interactive "*r")

  (if iedit-rectangle-mode
      (le::fix-indentation-extend (point))
    (iedit-rectangle-mode beg
                          ;; we pick the visual rectangle -- if end is (bol) then the visual rectangle
                          ;; does not include it.
                          (progn (goto-char end)
                                 (if (bolp)
                                     (progn (skip-chars-backward " \t\n")
                                            (point))
                                   end)))
    (le::fix-indentation-extend beg)
    (le::fix-indentation-extend end)))

(global-set-key (kbd "<C-S-return>") 'le::fix-indentation)