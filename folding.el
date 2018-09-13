(require 'yafolding)
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    ;;; Original defines
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    ;;; Custom defines for the rest
    (define-key map (kbd "<f8> z h") #'yafolding-hide-element)
    (define-key map (kbd "<f8> z s") #'yafolding-show-element)
    (define-key map (kbd "<f8> z t") #'yafolding-toggle-element)
    (define-key map (kbd "<f8> z H") #'yafolding-hide-all)
    (define-key map (kbd "<f8> z S") #'yafolding-show-all)
    (define-key map (kbd "<f8> z T") #'yafolding-toggle-all)
    (define-key map (kbd "<f8> z p") #'yafolding-go-parent-element)
    (define-key map (kbd "<f8> z P") #'yafolding-hide-parent-element)
    (define-key map (kbd "<f8> z z") #'yafolding-mode)
    map))
