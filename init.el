(setq default-directory "/home/chris")
(set-frame-font "Bitstream Vera Sans Mono")

;;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
(setq scroll-step 1) ; keyboard scroll one line at a time

;;; SLIME setup.
(setq inferior-lisp-program "/usr/bin/sbcl") ; I like SBCL, and this is where it lives.
(add-to-list 'load-path "/home/chris/slime/") ; This is my SLIME directory.
(require 'slime-autoloads)
(slime-setup)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono")))))
