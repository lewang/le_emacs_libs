(defvar le::custom-repos-sources
  '(("Agworld EmberApp" "app/assets/javascripts/app/")
    ("EmberJS" "~/src/ember/ember.js/")
    ("EmberData" "~/src/ember/data/")
    ("app" "app")
    ("app/controllers" "app/controllers/")
    ("app/models" "app/models/")
    ("app/views" "app/views/")
    ((lambda ()
       (format "root [%s]" (cdr (helm-cmd-t-root-data default-directory 'no-default))))
     (lambda ()
       (cdr (helm-cmd-t-root-data default-directory 'no-default)))))
  "(name root)  Any of which an be a function.

relative root will be converted to be relative to ")

(defun le::custom-repos ()
  (interactive)
  (let* ((base (helm-cmd-t-root))
         (options
          (delq nil
                (loop for row in le::custom-repos-sources
                      collect (let* ((name (nth 0 row))
                                     (root (nth 1 row)))
                                (when (functionp name)
                                  (setq name (funcall name)))
                                (setq root (cond ((functionp root)
                                                  (funcall root))
                                                 ((file-name-absolute-p root)
                                                  root)
                                                 (t
                                                  (expand-file-name root base))))
                                (when (file-directory-p root)
                                  (cons name (buffer-name (cdr (assq 'candidate-buffer (helm-cmd-t-get-create-source
                                                                                        (helm-cmd-t-make-root root))))))))))))
    (flet ((helm-cmd-t-get-caches () options))
      (helm-cmd-t-repos))))



