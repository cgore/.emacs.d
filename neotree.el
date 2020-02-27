;; Neotree - a nice directory listing panel on the left side of your Emacs.
(require 'neotree)
(global-set-key (kbd "<f8> <f8>") 'neotree-toggle)
(global-set-key (kbd "<f8> /") 'neotree-dir)
(global-set-key (kbd "<f8> .") 'neotree-find)
(setq neo-window-width 40)
(setq neo-autorefresh t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
