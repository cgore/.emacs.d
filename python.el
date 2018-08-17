(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(when (tcc-cgore?)
  (custom-set-variables '(py-shell-name "/usr/local/bin/python")))

(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation-mode)


;;; Conda mode - <https://github.com/necaris/conda.el>
(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
;;(conda-env-autoactivate-mode t)

(custom-set-variables
 '(conda-anaconda-home "/Users/chris.gore/anaconda"))
