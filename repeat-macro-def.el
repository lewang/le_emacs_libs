(provide repeat-macro-def)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A previous version of this function used repeat, but setting            ;;
;; `last-repeatable-command' caused the help text to show "&rest --cl-end" ;;
;; instead of the fomal arguments.  This is more functional any how.       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defun-repeatable (name args &rest body)
  "Define interactive function that can be repeated by pressing last key cord."
  (declare (indent defun))
  (let* ((res body)
         (doc-string (when (stringp (car res))
                       (list (pop res))))
         (interactive-form (if (eq 'interactive (caar res))
                               (pop res)
                             (error "definition is not interactive"))))
    `(defun ,name ,args
       ,@doc-string
       ,interactive-form
       (let* ((repeat-key (and (> (length (this-single-command-keys)) 1)
                               last-input-event))
              (repeat-key-str (format-kbd-macro (vector repeat-key) nil))
              event)
         (loop do ,(cons 'progn res)
               while (progn
                       (message "(Type %s to repeat)" repeat-key-str)
                       (setq event (read-event))
                       (clear-this-command-keys t)
                       (if (equal event repeat-key)
                           (progn
                             (setq last-input-event nil)
                             t)
                         (push last-input-event unread-command-events)
                         nil)))))))


(defmacro replace-repeatable (func)
  (let ((new-func (intern (concat (symbol-name func) "-repeatable"))))
    (progn
     `(defun-repeatable ,new-func ()
        ,(format "repeatable version of `%s'" func)
        (interactive)
        (call-interactively ,func))
     `(global-set-key [remap ,func] ',new-func))))
