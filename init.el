;;;; Copyright (c) 2013, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.


(let ((third-party "~/.emacs.d/third-party/"))
  (add-to-list 'load-path third-party)
  (mapcar #'(lambda (path)
	      (add-to-list 'load-path (concat third-party path)))
	  '("haml-mode"
	    "inf-ruby"
	    "org-mode"
	    "rails-reloaded")))


(setq tramp-default-method
      "ssh"

      default-directory
      "/home/chris"

      indent-tabs-mode
      nil ; Don't mix tabs and spaces, that is stupid.
      
      visible-bell
      t

      require-final-newline
      t

      next-line-add-newlines
      nil)

(display-time)


;;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ; three lines at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
(setq scroll-step 1) ; keyboard scroll one line at a time


;;; 80-wide for M-q.
(setq-default fill-column 80)

;;; C code setup.
(setq-default c-default-style "linux"
	      c-basic-offset 4)

;;; SLIME setup.
(setq inferior-lisp-program "/usr/bin/sbcl") ; I like SBCL, and this is where it lives.
(add-to-list 'load-path "/home/chris/programming/lisp/slime/") ; This is my SLIME directory.
(require 'slime-autoloads)
(slime-setup)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))


(when window-system ; Only if we are in a GUI.
  (set-foreground-color "white")
  (set-background-color "black")
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil
		  :stipple nil
		  :inverse-video nil
		  :box nil
		  :strike-through nil
		  :overline nil
		  :underline nil
		  :slant normal
		  :weight normal
		  :height 110
		  :width normal
		  :family "Droid Sans Mono")))))
  ;;; Color Themes.
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-subtle-hacker))


;;; Multi-Term
(require 'multi-term)
(setq multi-term-program "zsh")
;; TO DO: How do I look up the colors from the color theme directly?  I should
;; automate this somehow.  Multi-Term should probably be made aware of the color
;; theme if the library is present, assuming I can't already have it do that.
(when window-system
  (custom-set-variables
   '(term-default-bg-color "#2F4F4F")
   '(term-default-fg-color "#959882")
   '(term-bind-key-alist '(("C-c C-c" . term-interrupt-subjob)
			   ("C-p" . previous-line)
			   ("C-n" . next-line)
			   ("C-s" . isearch-forward)
			   ("C-r" . isearch-backward)
			   ("C-m" . term-send-raw)
			   ("M-f" . term-send-forward-word)
			   ("M-b" . term-send-backward-word)
			   ("M-o" . term-send-backspace)
			   ("M-p" . term-send-up)
			   ("M-n" . term-send-down)
			   ("M-M" . term-send-forward-kill-word)
			   ("M-N" . term-send-backward-kill-word)
			   ("M-r" . term-send-reverse-search-history)
			   ("M-," . term-send-input)
			   ("M-." . comint-dynamic-complete)
			   ;; These were defined in term but undefined by multi-term.
			   ;; I want them back.
			   ;;     -- cgore.
			   ("C-c C-j" . term-line-mode)
			   ("C-c C-k" . term-char-mode)))))

(defun bash ()
  (interactive)
  (let ((multi-term-program "bash")
	(multi-term-buffer-name "bash"))
    (multi-term)))

(defun irb ()
  (interactive)
  (let ((multi-term-program "irb")
	(multi-term-buffer-name "irb"))
    (multi-term)))

(defun python ()
  (interactive)
  (let ((multi-term-program "python")
	(multi-term-buffer-name "python"))
    (multi-term)))

(defun sbcl ()
  (interactive)
  (let ((multi-term-program "sbcl")
	(multi-term-buffer-name "sbcl"))
    (multi-term)))

(defun tig ()
  (interactive)
  (let ((multi-term-program "tig")
	(multi-term-program-switches "-a")
	(multi-term-buffer-name "tig"))
    (multi-term)))

(defun zsh ()
  (interactive)
  (let ((multi-term-program "zsh")
	(multi-term-buffer-name "zsh"))
    (multi-term)))

;;; Ruby stuff
(require 'ruby-mode)
(require 'yari) ; ri interface
(require 'haml-mode)
(require 'rails-autoload)


;;; ERC: Emacs IRC
(setq erc-log-channels-directory "~/.emacs.d/erc/log/"
      erc-nick "cgore"
      erc-user-full-name "Christopher Mark Gore"
      erc-email-userid "cgore@cgore.com"
      erc-save-buffer-on-part t
      erc-save-queries-on-quit t
      erc-log-write-after-send t
      erc-log-write-after-insert t)
(load "~/.emacs.d/ercpass")


;;; Org Mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-log-done t)


;;; Sunrise Commander - A Norton Commander clone
(require 'sunrise-commander)


;;; Web stuff.
(setq browse-url-browser-function 'w3m-browse-url)
(global-set-key (kbd "<f6> w") 'browse-url-at-point)
(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)
(defun browse-url-at-point-firefox ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url-at-point)))
(global-set-key (kbd "<f6> f") 'browse-url-at-point-firefox)
(defun drudge ()
  (interactive)
  (w3m-goto-url "http://www.drudgereport.com"))
(defun github ()
  (interactive)
  (w3m-goto-url "http://www.github.com"))
(defun google ()
  (interactive)
  (w3m-goto-url "http://www.google.com"))
(global-set-key (kbd "<f6> g")
		(lambda ()
		  (interactive)
		  (browse-url-firefox "http://www.google.com")))
(defun emacs-wiki ()
  (interactive)
  (w3m-goto-url "http://emacswiki.org"))


;;; TeX and LaTeX
(add-to-list 'auto-mode-alist '("\\.latex$" . latex-mode))


;;; Git
(add-to-list 'auto-mode-alist '("\\.gitconfig" . conf-mode))


;;; Greek Letters and Glyphs.
(global-set-key (kbd "<f9> a") "α")                                     
(global-set-key (kbd "<f9> b") "β")
(global-set-key (kbd "<f9> g") "γ")
(global-set-key (kbd "<f9> d") "δ")
; ...
(global-set-key (kbd "<f9> l") "λ")
; ...
(global-set-key (kbd "<f9> o") "ω")

(global-set-key (kbd "<f9> ->") "→")
(global-set-key (kbd "<f9> <-") "←")
(global-set-key (kbd "<f9> =>") "⇒")
(global-set-key (kbd "<f9> <=") "⇐")
(global-set-key (kbd "<f9> inf") "∞")

(global-set-key (kbd "<f9> :)") "☻")


;;; Buffers
(global-set-key (kbd "<f2> r") 'rename-buffer)
(put 'downcase-region 'disabled nil)
