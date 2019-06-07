;; This gives you the ability to cycle through different styles of variables
;; sort of like camel-snake-kebab for Clojure, switching between CamelCase,
;; snake_case, kebab-case, etc., just by pressing `C-c C-u`.

(require 'string-inflection)

(global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle)))
(add-hook 'java-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle)))
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-u") 'string-inflection-python-style-cycle)))
