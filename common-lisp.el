;;; -*- lexical-binding: t; -*-

(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-fancy))
(add-hook 'lisp-mode #'aggressive-indent-mode)
(setq lisp-indent-function 'common-lisp-indent-function)

(require 'slime-cl-indent)

(put :export                'common-lisp-indent-function 0)
(put :use                   'common-lisp-indent-function 0)
(put :import-from           'common-lisp-indent-function 0)
(put :shadow                'common-lisp-indent-function 0)
(put :nicknames             'common-lisp-indent-function 0)
(put :shadowing-import-from 'common-lisp-indent-function 0)

(put 'defsystem 'common-lisp-indent-function
     '(4 (&whole 4 &rest (&whole 2 &rest 2)) &rest 2))
