;;; TODO - I'm not sure why I've bothered using both yafolding and hideshow, I
;;; should probably just pick one and get rid of the other.

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

;;;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html>

(add-hook 'hs-minor-mode-hook
	  (lambda ()
	    (define-key hs-minor-mode-map (kbd "C-c h") 'hs-hide-block)
	    (define-key hs-minor-mode-map (kbd "C-c s") 'hs-show-block)
	    (define-key hs-minor-mode-map (kbd "C-c t") 'hs-toggle-hiding)
	    (define-key hs-minor-mode-map (kbd "C-c H") 'hs-hide-all)
	    (define-key hs-minor-mode-map (kbd "C-c S") 'hs-show-all)))

(add-hook 'clojure-mode-hook    'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'js-mode-hook         'hs-minor-mode)
(add-hook 'json-mode-hook       'hs-minor-mode)
(add-hook 'xml-mode-hook        'hs-minor-mode)
