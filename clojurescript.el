;;;; ClojureScript and Figwheel
;;
;; Copied and tweaked from:
;; https://github.com/bhauman/lein-figwheel/wiki/Running-figwheel-with-Emacs-Inferior-Clojure-Interaction-Mode
;;
;; C-c C-l will load the file.
;; C-c Meta-n will switch your namespace.
;; If you place your cursor at the end of a s-expression, C-x C-e will evaluate that expression.
;; C-c C-v will show the docs.
;; C-c C-s will show the source.
;; C-c <return> will macroexpand.
;;
;; More info on inf-clojure: https://github.com/clojure-emacs/inf-clojure
(defun lein-figwheel ()
  (interactive)
  (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
  (inf-clojure-minor-mode)
  (inf-clojure "lein figwheel"))
