;;; JavaScript and JSON
(when (or (linux?) (darwin?))
  (require 'json-mode))
(setq js-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.geojson$" . json-mode))
