;;; Curry, rcurry, compose, pretty-curry-compose, all from:
;;;  https://gist.github.com/eschulte/6167923

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

 (defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(defsubst compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))

;;; compact display
;; (defun pretty-curry-compose ()
;;   (mapc (lambda (pair)
;;           (let ((regexp (car pair))
;;                 (symbol (cdr pair)))
;;             (font-lock-add-keywords 'emacs-lisp-mode
;;               `((,regexp
;;                  (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                            ,symbol)
;;                            nil)))))))
;;         '(("(\\(compose\\)[ \t\n\r]" . ?\∘)
;;           ("(\\(curry\\)[ \t\n\r]" . ?\»)
;;           ("(\\(rcurry\\)[ \t\n\r]" . ?\«))))
;; (add-to-list 'emacs-lisp-mode-hook 'pretty-curry-compose)

;;; color these functions like keywords
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(compose\\)[ \t\n\r]" 1 font-lock-keyword-face)
                          ("(\\(curry\\)[ \t\n\r]" 1 font-lock-keyword-face)
                          ("(\\(rcurry\\)[ \t\n\r]" 1 font-lock-keyword-face)))
