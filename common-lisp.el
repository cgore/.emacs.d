(setq inferior-lisp-program "sbcl")
(add-hook 'lisp-mode #'aggressive-indent-mode)

;; (setq inferior-lisp-program "/usr/bin/env sbcl --noinform --no-linedit")

;; (load (expand-file-name "~/.roswell/helper.el"))

;; (setq sly-lisp-implementations
;;       (append (cl-remove-if-not (lambda (pair) (executable-find (caadr pair)))
;;                                 '((lispworks ("lw-console"))
;;                                   (sbcl ("sbcl"))
;;                                   (ccl-x64 ("ccl64"))
;;                                   (ccl-arm ("armcl"))
;;                                   (ecl ("ecl"))
;;                                   (clisp ("clisp"))))
;;               (let ((path (locate-file "abcl.jar" exec-path)))
;;                 (when path
;;                   `((abcl ("java" "-jar" ,path)))))))
