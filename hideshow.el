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
