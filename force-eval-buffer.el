;; URL: https://github.com/lewang/le_emacs_libs/blob/master/force-eval-buffer.el
;;
;;
;; this evaluates a buffer, but changing all variable definition to setq to
;; force reevaluation.  It's useful if you want to reset to a library's
;; defaults without restarting Emacs

;;;;;;;;;;;; these are stupid alternatives I tried before

(defmacro replacement-def (sym &optional val &rest args)
  `(setq ,sym ,val))

;;;###autoload
(defun force-eval-buffer (&optional buf)
  (interactive)
  (eval-buffer buf)                         ; define variables properly at least once
  (let ((func-alist (mapcar (lambda (func)
                              (cons func (symbol-function func)))
                            '(defvar defcustom))))
    (unwind-protect
        (progn
          (mapc (lambda (func-cons)
                  (fset (car func-cons) (symbol-function 'replacement-def)))
                func-alist)
          (eval-buffer buf))
      (mapcar (lambda (func-cons)
                (fset (car func-cons) (cdr func-cons)))
              func-alist))))
