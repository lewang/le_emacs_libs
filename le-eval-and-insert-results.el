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
