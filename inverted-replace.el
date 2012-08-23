(provide 'inverted-replace)

(require 're-builder)
(require 'parallel-replace)

(defun inverted-replace-generate-replacement (from to)
  "invert result of current match (match-string 0)"
  (let ((string (match-string 0))
        (count (reb-count-subexps from))
        (replacements (parallel-replace-read-list to)))
    (save-match-data
      (string-match from string)
      (dotimes (i count)
        (setq string (replace-match (nth i replacements) nil nil string (- count i)))))
    string))

(defun inverted-replace-regexp (from to)
  (interactive (destructuring-bind (from to _)
                   (query-replace-read-args "inverted-replace-regexp: " t)
                 (list from to)))
  (query-replace-regexp from
                        (quote (replace-eval-replacement
                                replace-quote
                                (inverted-replace-generate-replacement from to)))
                        nil (and (and transient-mark-mode mark-active)
                               (region-beginning))
                        (and (and transient-mark-mode mark-active) (region-end))))
