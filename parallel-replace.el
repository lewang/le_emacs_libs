(provide 'parallel-replace)

;;; -lw- 2012-01-28 The main reason for keeping this is the smart way it
;;; parses input from minibuffer

;; http://stackoverflow.com/a/2592685/903943

(require 'cl)

(defun parallel-replace-read-plist (input)
  (loop with limit = (length input)
        for (item . index) = (read-from-string input 0)
        then (read-from-string input index)
        collect (prin1-to-string item t) until (<= limit index)))

(defun parallel-replace (plist &optional start end)
  (interactive
   (cons
    (parallel-replace-read-plist (read-from-minibuffer "Replace: "))
    (when (use-region-p)
      (list (region-beginning) (region-end)))))
  (let* ((alist (loop for (key val . tail) on plist by #'cddr
                      collect (cons key val)))
         (matcher (regexp-opt (mapcar #'car alist) 'words)))
    (save-excursion
      (goto-char (or start (point)))
      (while (re-search-forward matcher (or end (point-max)) t)
        (replace-match (cdr (assoc-string (match-string 0) alist)))))))

(defvar parallel-replace-alist nil)

(defun parallel-query-replace (plist &optional start end)
  (interactive
   (cons
    (parallel-replace-read-plist (read-from-minibuffer "Replace: "))
    (when (use-region-p)
      (list (region-beginning) (region-end)))))
  (let* (matcher)
    (set (make-local-variable 'parallel-replace-alist)
         (loop for (key val . tail) on plist by #'cddr
               collect (cons key val)))
    (setq matcher (regexp-optp (mapcar #'car parallel-replace-alist) 'words))
    (query-replace-regexp matcher
                          '(replace-eval-replacement
                            replace-quote
                            (cdr (assoc-string (match-string 0) parallel-replace-alist case-fold-search)))
                          nil
                          start
                          end)))

