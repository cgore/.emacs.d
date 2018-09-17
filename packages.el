(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(require 'package)
(setq package-list '(ace-jump-mode
                     ack-and-a-half
                     auto-complete
                     auto-highlight-symbol
                     base16-theme
                     cider
                     clojure-cheatsheet
                     clojure-mode
                     clj-refactor
                     coffee-mode
                     color-theme-sanityinc-tomorrow
                     conda
                     csharp-mode
                     cyberpunk-theme
                     elpy
                     eshell-autojump
                     flycheck
                     flycheck-clojure
                     flycheck-pos-tip
                     flymake-go
                     go-mode
                     gorepl-mode
                     haml-mode
                     highlight-indentation
                     indium
                     inf-clojure
                     inf-ruby
                     js2-refactor
                     json-mode
                     kibit-helper
                     logview
                     magit
                     magit-gitflow
                     markdown-mode
                     mc-extras
                     multi-term
                     multiple-cursors
                     neotree
                     nodejs-repl
                     ob-restclient
                     paper-theme
                     paredit
                     puppet-mode
                     projectile
                     python-mode
                     queue
                     rainbow-delimiters
                     rainbow-mode
                     restclient
                     reverse-theme
                     rjsx-mode
                     robe
                     rspec-mode
                     ruby-electric
                     sass-mode
                     scss-mode
                     xref-js2
                     w3m
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
