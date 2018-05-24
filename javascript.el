;;; JavaScript and JSON
(when (or (linux?) (darwin?))
  (require 'json-mode))
(require 'rjsx-mode)
(setq js-indent-level 4 ; js-mode
      javascript-indent-level 4 ; javascript-mode
      web-mode-markup-indent-offset 4
      web-mode-markup-indent-offset 4  ; web-mode, html tag in html file
      web-mode-css-indent-offset 4 ; web-mode, css in html file
      web-mode-code-indent-offset 4  ; web-mode, js code in html file
      css-indent-level 4)  ; css-mode


(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.geojson$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
