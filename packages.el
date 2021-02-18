(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(require 'package)
(setq package-list '(ace-jump-mode
		     aggressive-indent
                     all-the-icons
                     all-the-icons-dired
                     auto-complete
                     auto-highlight-symbol
                     base16-theme
                     beacon
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
                     doom-themes
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
                     ht
                     indium
                     inf-clojure
                     inf-ruby
                     jq-format
                     jq-mode
                     js2-refactor
                     json-mode
                     kibit-helper
                     kubernetes ; https://github.com/chrisbarrett/kubernetes-el
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
                     rainbow-delimiters
                     rainbow-mode
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
                     sly
                     string-inflection
                     terraform-doc
                     terraform-mode
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
