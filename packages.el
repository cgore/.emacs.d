;;; -*- lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")    t)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)

;; (setq package-archives
;;       '(;;("ELPA"         . "http://tromey.com/elpa/")
;;         ("gnu"          . "https://elpa.gnu.org/packages/")
;;         ("melpa"        . "https://melpa.org/packages/")
;;         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
;;         ;;("marmalade"    . "http://marmalade-repo.org/packages/")
;;         ))

(setq package-install-upgrade-built-in t)

(setq package-list '(ace-jump-mode
		                 aggressive-indent
                     all-the-icons
                     all-the-icons-dired
                     auto-complete
                     auto-highlight-symbol
                     base16-theme
                     cider
                     clojure-mode
                     clj-refactor
                     coffee-mode
                     color-theme-sanityinc-tomorrow
                     conda
                     csharp-mode
                     cyberpunk-theme
                     dash
                     dash-functional
                     elpy
                     eshell-autojump
                     highlight-indentation
                     ht
                     indium
                     inf-clojure
                     inf-ruby
                     jq-format
                     jq-mode
                     js2-refactor
                     json-mode
                     kibit-helper
                     logview
                     magit
                     markdown-mode
                     mc-extras
                     multi-term
                     multiple-cursors
                     neotree
                     nodejs-repl
                     ob-restclient
                     omnisharp
                     paper-theme
                     paredit
                     puppet-mode
                     projectile
                     python-mode
                     queue
                     restclient
                     reverse-theme
                     rjsx-mode
                     robe
                     rspec-mode
                     ruby-electric
                     rust-mode
                     s
                     sass-mode
                     solidity-mode
                     scss-mode
                     string-inflection
                     terraform-doc
                     terraform-mode
                     tide
                     typescript-mode
                     uuidgen
                     xref-js2
                     w3m
                     which-key
                     yafolding
                     yaml-mode))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(setq url-http-attempt-keepalives nil)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))


;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name
;;         "straight/repos/straight.el/bootstrap.el"
;;         (or (bound-and-true-p straight-base-dir)
;;             user-emacs-directory)))
;;       (bootstrap-version 7))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
