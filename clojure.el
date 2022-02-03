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

(define-prefix-command 'sexp-movements-map)
(global-set-key (kbd "<f5>") 'sexp-movements-map)
(define-key sexp-movements-map (kbd "<left>") 'backward-sexp)
(define-key sexp-movements-map (kbd "<right>") 'forward-sexp)
(define-key sexp-movements-map (kbd "<up>") 'backward-up-list)
(define-key sexp-movements-map (kbd "<down>") 'up-list)

(eval-after-load 'flycheck '(flycheck-clojure-setup))

(require 'flycheck-clj-kondo)

(global-set-key (kbd "C-c M-c") 'cider-connect)
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-repl-display-help-banner nil)
(setq cider-repl-result-prefix ";; ⇒ ")
(setq cider-interactive-eval-result-prefix ";; ⟶ ")
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
;; List of available refactorings: <https://github.com/clojure-emacs/clj-refactor.el/wiki>
(require 'clj-refactor)
(defun clj-refactor-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
(setq cljr-warn-on-eval nil)
(add-hook 'clojure-mode-hook #'clj-refactor-mode-hook)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (OPTIONS 2)
  (PATCH 2)
  (rfn 2)
  (let-routes 1)
  (context 2))
