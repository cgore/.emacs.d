(add-to-list 'auto-mode-alist '("\\.gitconfig" . conf-mode))
(global-set-key (kbd "<f2> g") 'magit-status)
(global-set-key (kbd "<f2> l") 'magit-log-current)
(global-set-key (kbd "<f2> t") 'magit-log-all)
(setq magit-last-seen-setup-instructions "1.4.0")
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; https://github.com/magit/with-editor
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
