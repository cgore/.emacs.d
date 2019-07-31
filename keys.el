(global-set-key (kbd "<f6> c") 'calc)

(global-unset-key (kbd "C-t"))
(defvar ctrl-t-map (make-sparse-keymap))
(global-set-key (kbd "C-t") ctrl-t-map)
(global-set-key (kbd "C-t s") 'copy-to-register)
(global-set-key (kbd "C-t i") 'insert-register)
(global-set-key (kbd "C-t >") 'append-to-register)
(global-set-key (kbd "C-t <") 'prepend-to-register)
