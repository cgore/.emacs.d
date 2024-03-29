(require 'inf-ruby)
(require 'robe)
(require 'rspec-mode)
(require 'ruby-electric)
(require 'ruby-mode)
(require 'yari) ; ri interface
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle 'overlay)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(setq ruby-deep-arglist nil)
(setq ruby-deep-indent-paren nil)
(add-hook 'ruby-mode-hook 'robe-mode)
(setq rspec-use-rake-when-possible nil)
(require 'haml-mode)
(require 'sass-mode)
(require 'scss-mode)
(setq scss-compile-at-save nil)

(when (darwin?)
  ;; Switch to the brew ruby from `brew install ruby`
  (setq exec-path
        (append (list "/usr/local/opt/ruby/bin")
                exec-path))
  (add-to-ldflags   "-L/usr/local/opt/ruby/lib")
  (add-to-cppflags  "-I/usr/local/opt/ruby/include"))
