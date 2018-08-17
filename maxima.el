(add-to-list 'load-path "/usr/share/maxima/5.32.1/emacs/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t
      imaxima-pt-size 12
      imaxima-fnt-size "large"
      imaxima-max-scale nil
      imaxima-linearize-flag nil)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))
(global-set-key (kbd "<XF86Calculator>") 'maxima)
