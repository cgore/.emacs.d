;;;; Copyright © 2013-2014, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 2317 South River Road, Saint Charles, Missouri 63303 USA.
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
          '("curry-gist"
            "emacs-soap-client"
            "haml-mode"
            "inf-ruby"
            "json-mode"
            "org-mode/lisp"
            "org-jira"
            "php-mode"
            "rails-reloaded"
            "ruby-electric"
            "scss-mode")))

(load "~/.emacs.d/utilities.el")

(require 'ido)
(ido-mode t)

(defun cygwin? ()
  (eq system-type 'cygwin))
(defun linux? ()
  (eq system-type 'gnu/linux))

(defun abaddon? () ; work
  (string= system-name "abaddon"))
(defun corinth? () ; laptop
  (string= system-name "corinth"))
(defun ezekiel? () ; California
  (string= system-name "ezekiel"))
(defun habakkuk? () ; home
  (string= system-name "habakkuk"))
(defun naaman? () ; Atlanta
  (string= system-name "naaman"))

(defun fixed-buffer-width ()
  (cond ((not window-system) 78)
        ((abaddon?)          100)
        ((corinth?)          80)
        ((habakkuk?)         100)
        (t                   80)))

(setq-default c-basic-offset 4
              c-default-style "linux"
              fill-column 80) ; 80-wide for M-q.

(setq compilation-scroll-output t
      default-directory "~"
      indent-tabs-mode nil ; Don't mix tabs and spaces, that is stupid.
      inferior-lisp-program "/usr/bin/sbcl" ; I like SBCL
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

(blink-cursor-mode 0)

(setenv "MANWIDTH" (number-to-string (fixed-buffer-width)))

;;; SLIME setup.
(when (linux?)
  (add-to-list 'load-path "~/programming/lisp/slime/") ; My SLIME directory.
  (require 'slime-autoloads)
  (slime-setup))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(display-time-day-and-date t)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(socks-server '("default" "localhost" 9999 5))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(jiralib-url "http://dev-task")
 '(woman-fill-column (fixed-buffer-width))
 '(woman-fill-frame t)
 '(woman-bold-headings t))

(when (abaddon?)
 (setq socks-noproxy '("localhost"
                       "chat"
                       "dev-task"
                       "dev-wiki"))
 (require 'socks)
 (setq erc-server-connect-function 'socks-open-network-stream))

(when (and window-system (linux?))
  (custom-set-faces
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
  (color-theme-initialize))

;;; Multi-Term
(require 'multi-term)
(setq multi-term-program "zsh")
(defun term-send-escape ()
  (interactive)
  (term-send-raw-string "\e"))

(defun reset-term-colors ()
  (interactive)
  (set-foreground-color (face-foreground 'default))
  (set-background-color (face-background 'default))
  (custom-set-variables
   '(term-default-bg-color (face-background 'default))
   '(term-default-fg-color (face-foreground 'default))))

(when window-system
  (reset-term-colors)
  (custom-set-variables
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
                           ;; These were defined in term but undefined by
                           ;; multi-term.  I want them back.
                           ("C-c C-j" . term-line-mode)
                           ("C-c C-k" . term-char-mode)
                           ("C-c C-e" . term-send-escape)))))

(defun dark-colors ()
  (interactive)
  (when (and window-system (linux?))
    (color-theme-charcoal-black)
    (reset-term-colors)))

(defun light-colors ()
  (interactive)
  (when (and window-system (linux?))
    (color-theme-gtk-ide)
    (reset-term-colors)))

(dark-colors)

(defun bash ()
  (interactive)
  (let ((multi-term-program "bash")
        (multi-term-buffer-name "bash"))
    (multi-term)))

;;; Normally you should just use inf-ruby.
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

;;; Normally you should just use SLIME.
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
(global-set-key (kbd "<f2> t") 'tig)

(defun zsh ()
  (interactive)
  (let ((multi-term-program "zsh")
        (multi-term-buffer-name "zsh"))
    (multi-term)))
(global-set-key (kbd "<f2> z") 'zsh)

(defun su ()
  (interactive)
  (let ((multi-term-program "su")
        (multi-term-buffer-name "su")
        (multi-term-program-switches "--login"))
    (multi-term)))
(global-set-key (kbd "<f6> r") 'su)

(defun ssh (ssh-to)
  (interactive "sSSH to: ")
  (let ((multi-term-program "ssh")
        (multi-term-buffer-name ssh-to)
        (multi-term-program-switches ssh-to))
    (multi-term)))
(global-set-key (kbd "<f6> s") 'ssh)

(defun ri (query)
  (interactive "sRI Query: ")
  (let ((multi-term-program "ri")
        (multi-term-buffer-name (concat "ri " query))
        (multi-term-program-switches query))
    (multi-term)))


;;; JavaScript and JSON
(when (linux?)
  (require 'json-mode))


;;; Ruby
(require 'ruby-electric)
(require 'ruby-mode)
(require 'yari) ; ri interface
(when (linux?)
  (require 'inf-ruby))
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle 'overlay)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(setq ruby-deep-arglist nil)
(setq ruby-deep-indent-paren nil)

;;; Ruby on Rails
(when (linux?)
  (require 'haml-mode)
  (require 'rails-autoload)
  (setq scss-compile-at-save nil)
  (require 'scss-mode))


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
      erc-fill-column (fixed-buffer-width)
      erc-autojoin-channels-alist '(("chat"
                                     "#chat" "#slamr-dev")
                                    ("freenode.net"
                                     "#jesus" "#lisp" "#ruby")))
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
(when (or (habakkuk?) (abaddon?))
  (load "~/.emacs.d/ercpass"))


;;; Org Mode
(require 'org-install)
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-log-done t)
(add-hook 'org-mode-hook 'org-indent-mode)


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
      w3m-terminal-coding-system 'utf-8
      w3m-use-cookies t)
(defun browse-url-at-point-firefox ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url-at-point)))
(global-set-key (kbd "<f6> f") 'browse-url-at-point-firefox)
(global-set-key (kbd "<f6> g")
                (icurry 'browse-url-firefox "http://www.google.com"))

(load "~/.emacs.d/thinking-bicycle.el")

;;; TeX and LaTeX
(add-to-list 'auto-mode-alist '("\\.latex$" . latex-mode))


;;; Git
(add-to-list 'auto-mode-alist '("\\.gitconfig" . conf-mode))
(global-set-key (kbd "<f2> g") 'magit-status)


;;; Buffer navigation.
(global-set-key (kbd "<XF86Back>") 'previous-buffer)
(global-set-key (kbd "<XF86Forward>") 'next-buffer)


;;; Odd letters and glyphs.
(defun french-input ()
  (interactive)
  (set-input-method "french-postfix"))
(defun greek-input ()
  (interactive)
  (set-input-method "greek"))
(defun ucs-input ()
  (interactive)
  (set-input-method "ucs"))
(defun tex-input ()
  (interactive)
  (set-input-method "TeX"))

(global-set-key (kbd "<f9> f") 'french-input)
(global-set-key (kbd "<f9> g") 'greek-input)
(global-set-key (kbd "<f9> t") 'tex-input)
(global-set-key (kbd "<f9> u") 'ucs-input)

(global-set-key (kbd "<f9> c") "¢")
(global-set-key (kbd "<f9> B") "฿")
(global-set-key (kbd "<f9> E") "€")
(global-set-key (kbd "<f9> L") "£")

(global-set-key (kbd "<f9> C") "©")
(global-set-key (kbd "<f9> R") "®")
(global-set-key (kbd "<f9> TM") "™")

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


;;; Rectangles
(global-set-key (kbd "C-x r I") 'string-insert-rectangle)


;;; GPG stuff.
(require 'epa-file)
;; With this, you can find-file something.gpg and it will just work.
(epa-file-enable)

;;; Whitespace mode
(require 'whitespace)
(setq whitespace-style '(face empty tabs trailing))
(global-whitespace-mode 0)
(mapcar (lambda (mode-hook)
          (add-hook mode-hook 'whitespace-mode))
        '(c-mode-hook
          c++-mode-hook
          emacs-lisp-mode-hook
          lisp-mode-hook
          python-mode-hook
          ruby-mode-hook))

;;; Eshell Functions

;; Taken from <http://www.emacswiki.org/emacs/EshellFunctions>
(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun format-commands (&rest commands)
  (mapcar (lambda (command)
            (shell-command (apply 'format command)))
          commands))

(defun eshell/git-branch-here (branch)
  (format-commands `("git push origin :%s" ,branch)
                   `("git branch -d %s" ,branch)
                   `("git checkout -b %s" ,branch)
                   `("git push origin %s:%s" ,branch ,branch)))

(defun eshell/git-experimental-here ()
  (eshell/git-branch-here "experimental"))

;;; Atlassian stuff
(when (abaddon?)
  (setq jirilib-url "http://dev-task/")
  (require 'org-jira)
  (global-set-key (kbd "<f5> j")
                  (icurry 'w3m-browse-url "http://dev-task"))
  (global-set-key (kbd "<f5> w")
                  (icurry 'w3m-browse-url "http://dev-wiki")))

;;; Spell Checking
(mapcar #'(lambda (mode-hook)
            (add-hook mode-hook 'flyspell-mode))
        '(latex-mode-hook
          magit-log-edit-mode-hook
          org-mode-hoook))
(mapcar #'(lambda (mode-hook)
            (add-hook mode-hook 'flyspell-prog-mode))
        '(c-mode-hook
          c++-mode-hook
          emacs-lisp-mode-hook
          lisp-mode-hook
          org-mode-hook
          php-mode-hook
          python-mode-hook
          ruby-mode-hook))

;;; Maxima
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


;;; PHP
(require 'php-mode)
