;;; JavaScript and JSON
(require 'js2-mode)
(require 'js2-refactor)
(require 'xref-js2)
(require 'rjsx-mode)
(setq js-indent-level 2 ; js-mode
      javascript-indent-level 2 ; javascript-mode
      web-mode-markup-indent-offset 2
      web-mode-markup-indent-offset 2  ; web-mode, html tag in html file
      web-mode-css-indent-offset 2 ; web-mode, css in html file
      web-mode-code-indent-offset 2  ; web-mode, js code in html file
      css-indent-level 2)  ; css-mode


(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.geojson$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))

;; NodeJS stuff

(require 'nodejs-repl)
(add-hook 'js-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "C-c M-j") 'nodejs-repl)
            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            (define-key js-mode-map (kbd "C-c C-k") 'nodejs-repl-send-buffer)
            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))
