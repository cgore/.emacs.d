(when (linux?)
  ;; For some reason the Debian-modified leiningen has OpenJDK java as their
  ;; default, unless you set this environment variable.  I probably want to use
  ;; Oracle's Java most of the time.
  (setenv "JAVA_CMD" "/usr/bin/java"))

(dolist (p '(auto-complete
             clojure-mode
             cider
             cider-test))
  (require p))
(global-set-key (kbd "C-c M-c") 'cider-connect)
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-repl-result-prefix ";; => ")
(setq cider-interactive-eval-result-prefix ";; -> ")
(setq cider-repl-history-size 10000)
(setq cider-repl-history-file (expand-file-name "~/.emacs.d/cider-repl.history"))
(add-hook 'clojure-mode-hook 'cider-mode)
(eval-after-load 'cider
  '(progn
     (add-hook 'cider-repl-mode-hook 'paredit-mode)
     (local-set-key (kbd "M-<return>") 'newline)))

(global-set-key (kbd "M-<return>") 'newline)
(global-set-key (kbd "C-x C-j") 'eval-print-last-sexp) ; paredit squashes C-j

;; <https://github.com/clojure-emacs/clj-refactor.el>
(require 'clj-refactor)
(defun clj-refactor-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
(setq cljr-warn-on-eval nil)
(add-hook 'clojure-mode-hook #'clj-refactor-mode-hook)


(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
