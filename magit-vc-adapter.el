(eval-when-compile
  (require 'cl))

(defvar le::vc-diff-data nil)

(defun le::magit-go-to-change-once ()
  (destructuring-bind (filename orig-buff relative-name) le::vc-diff-data
    (pop-to-buffer "*magit: magit*")
    (goto-char (point-min))
    (if (and (search-forward-regexp "^Changes:$" nil t)
             (progn (magit-show-level-2-all) t)
             (search-forward relative-name nil t))
        (progn (recenter-top-bottom 0)
               (magit-show-level-4))
      ;; no diff
      (pop-to-buffer orig-buf)
      (message "no diff found.")))
  (remove-hook 'magit-refresh-status-hook #'le::magit-go-to-change-once))

(defadvice vc-diff (around magit-redirect activate compile)
  "redirect to magit"
  (let* ((vc-info (vc-deduce-fileset t))
         (filename (buffer-file-name))
         (orig-buf (current-buffer))
         (relative-name (replace-regexp-in-string
                         (concat "\\`"
                                 (regexp-quote (expand-file-name (locate-dominating-file filename ".git"))))
                         "" filename)))
    (if (string-equal "Git" (car vc-info))
        (progn
          (setq le::vc-diff-data (list filename orig-buf relative-name))
          (add-hook 'magit-refresh-status-hook #'le::magit-go-to-change-once)
          (call-interactively 'magit-status))
      ad-do-it)))

(provide 'magit-vc-adapter)

