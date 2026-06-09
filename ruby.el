;;; -*- lexical-binding: t; -*-

;; === Core Ruby support ===
(require 'ruby-mode)  ; built-in, or prefer ruby-ts-mode if available

;; Prefer Tree-sitter mode when possible (Emacs 29+)
(when (and (fboundp 'ruby-ts-mode)
           (treesit-ready-p 'ruby t))
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

;; Interactive Ruby (REPL) - still excellent and actively used
(require 'inf-ruby)

(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; RSpec support
(require 'rspec-mode)
(setq rspec-use-rake-when-possible nil)

;; Robe (good for navigation/completion on non-LSP projects)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

;; Yari (RI documentation) - still useful
(require 'yari)
(define-key 'help-command (kbd "R") #'yari)  ; optional: C-h R for Ruby docs

;; === Modern replacements for older packages ===

;; ruby-electric → use built-in electric-pair-mode (global or per-mode)
(electric-pair-mode 1)  ; enable globally, or add to ruby-mode-hook only

;; ruby-block → no longer needed with modern ruby-mode / ruby-ts-mode
;; (highlighting of matching blocks/end keywords works better now)

;; === Haml / Sass / SCSS ===

(require 'haml-mode)

;; Fix for old scss-mode + modern Flymake
(require 'flymake-proc nil t)
;; Modern Sass/SCSS: use built-in css-mode + scss-mode, or web-mode
(require 'scss-mode)
(setq scss-compile-at-save nil)  ; keep your preference

;; Optional: better SCSS support with web-mode (recommended)
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.sass\\'" . web-mode))

;; === File associations ===
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))

;; === Indentation tweaks (modern defaults are usually better) ===
(setq ruby-deep-arglist nil)
(setq ruby-deep-indent-paren nil)

;; Optional: cleaner encoding comments
(setq ruby-insert-encoding-magic-comment nil)

