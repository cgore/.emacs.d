;;; ERC: Emacs IRC
(setq erc-log-channels-directory "~/.emacs.d/erc/log/"
      erc-nick "cgore"
      erc-user-full-name "Christopher Mark Gore"
      erc-email-userid "cgore@cgore.com"
      erc-interpret-mirc-color t
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-save-buffer-on-part t
      erc-save-queries-on-quit t
      erc-log-write-after-send t
      erc-log-write-after-insert t
      erc-autojoin-channels-alist '(("freenode.net"
                                     "#clojure" "#jesus" "#lisp")))

(setq erc-prompt
      (lambda ()
        (erc-propertize (if (and (boundp 'erc-default-recipients)
                                 (erc-default-target))
                          (concat (erc-default-target) ">")
                          (concat "ERC>"))
                        'read-only t 'rear-nonsticky t 'front-nonsticky t)))
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(add-to-list 'erc-modules 'log)
(erc-update-modules)

;; Set your erc-nickserv-passwords in this file.  Example:
;;(setq erc-nickserv-passwords
;;      `((freenode (("whoYouAre" . "yourSecretPassword")))))
(setq ercpass-el "~/.emacs.d/ercpass.el")
(when (file-exists-p ercpass-el)
  (load "~/.emacs.d/ercpass.el"))
