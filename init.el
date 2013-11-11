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

(server-start)

(let ((third-party "~/.emacs.d/third-party/"))
  (add-to-list 'load-path third-party)
  (mapcar #'(lambda (path)
              (add-to-list 'load-path (concat third-party path)))
          '("emacs-soap-client"
            "haml-mode"
            "inf-ruby"
            "org-mode/lisp"
            "org-jira"
            "rails-reloaded")))
(add-to-list 'load-path "/home/chris/programming/lisp/slime/") ; This is my SLIME directory.

(setq-default c-basic-offset 4
              c-default-style "linux"
              fill-column 80) ; 80-wide for M-q.

(setq compilation-scroll-output t
      default-directory "/home/chris"
      indent-tabs-mode nil ; Don't mix tabs and spaces, that is stupid.
      inferior-lisp-program "/usr/bin/sbcl" ; I like SBCL, and this is where it lives.
      mouse-wheel-scroll-amount '(3 ((shift) . 3)) ; three lines at a time
      mouse-wheel-follow-mouse t ; scroll window under mouse
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      next-line-add-newlines nil
      require-final-newline t
      scroll-step 1 ; keyboard scroll one line at a time
      tramp-default-method "ssh"
      visible-bell t)
(setq-default indent-tabs-mode nil)
(put 'scroll-left 'disabled nil)

;;; Toolbar setup
(display-time)
(column-number-mode 1)

;;; 80-wide for man pages
(setenv "MANWIDTH" "80")

;;; SLIME setup.
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
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(jiralib-url "http://dev-task"))


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
                  :height 120
                  :width normal
                  :family "Droid Sans Mono")))))
  ;;; Color Themes.
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-tty-dark))

;;; Multi-Term
(require 'multi-term)
(setq multi-term-program "zsh")
(defun term-send-escape ()
  (interactive)
  (term-send-raw-string "\e"))
;; TO DO: How do I look up the colors from the color theme directly?  I should
;; automate this somehow.  Multi-Term should probably be made aware of the color
;; theme if the library is present, assuming I can't already have it do that.
(when window-system
  (custom-set-variables
   '(term-default-bg-color "#000000")
   '(term-default-fg-color "#EEEEEE")
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
                           ("C-c C-j" . term-line-mode)
                           ("C-c C-k" . term-char-mode)
                           ("C-c C-e" . term-send-escape)))))

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

(defun ssh (ssh-to)
  (interactive "sSSH to: ")
  (let ((multi-term-program "ssh")
        (multi-term-buffer-name ssh-to)
        (multi-term-program-switches ssh-to))
    (multi-term)))
(global-set-key (kbd "<f6> s") 'ssh)

;;; Ruby stuff
(require 'ruby-mode)
(require 'yari) ; ri interface
(require 'haml-mode)
(require 'rails-autoload)
(require 'inf-ruby)

;;; ERC: Emacs IRC
(setq erc-log-channels-directory "~/.emacs.d/erc/log/"
      erc-nick "cgore"
      erc-user-full-name "Christopher Mark Gore"
      erc-email-userid "cgore@cgore.com"
      erc-save-buffer-on-part t
      erc-save-queries-on-quit t
      erc-log-write-after-send t
      erc-log-write-after-insert t)
(setq erc-prompt
      (lambda ()
        (erc-propertize (if (and (boundp 'erc-default-recipients)
                                 (erc-default-target))
                          (concat (erc-default-target) ">")
                          (concat "ERC>"))
                        'read-only t 'rear-nonsticky t 'front-nonsticky t)))
(load "~/.emacs.d/ercpass")


;;; Org Mode
(require 'org-install)
(require 'org)
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


(global-set-key (kbd "<XF86Back>") 'previous-buffer)
(global-set-key (kbd "<XF86Forward>") 'next-buffer)

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
(put 'narrow-to-region 'disabled nil)

;;; GPG stuff.
(require 'epa-file)
;; With this, you can find-file something.gpg and it will just work.
(epa-file-enable)

;;; Whitespace mode
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing)
      whitespace-line-column 80)
(global-whitespace-mode nil)
(mapcar (lambda (mode-hook)
          (add-hook mode-hook 'whitespace-mode))
        '(c-mode-hook
          lisp-mode-hook
          python-mode-hook
          ruby-mode-hook))


;;; Org Jira
(when (string= system-name "abaddon")
  (setq jirilib-url "http://dev-task/")
  (require 'org-jira))
