(global-set-key (kbd "<f6> +") 'calc)

(global-unset-key (kbd "C-t"))
(defvar ctrl-t-map (make-sparse-keymap))
(global-set-key (kbd "C-t") ctrl-t-map)
(global-set-key (kbd "C-t s") 'copy-to-register)
(global-set-key (kbd "C-t i") 'insert-register)
(global-set-key (kbd "C-t >") 'append-to-register)
(global-set-key (kbd "C-t <") 'prepend-to-register)

;; The Windows Apps key - shows up as C-p for some reason on mac.
(when (darwin?)
  (global-set-key (kbd "C-p") 'execute-extended-command))
(when (cygwin?)
  (global-set-key (kbd "<apps>") 'execute-extended-command))

;;; https://github.com/justbur/emacs-which-key
(require 'which-key)
(which-key-mode)

;;; Default: minibuffer
;;; I don't know how to use paging with the minibuffer variant.
;; (which-key-setup-minibuffer)
;;; Optional: bottom window or right window
;; (which-key-setup-side-window-right)
(which-key-setup-side-window-bottom)
(global-set-key (kbd "<f7> k") 'which-key-show-top-level)
