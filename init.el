;;;; Copyright © 2013-2016, Christopher Mark Gore,
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


(defun cygwin? ()
  (eq system-type 'cygwin))
(defun darwin? ()
  (eq system-type 'darwin))
(defun linux? ()
  (eq system-type 'gnu/linux))

(defun abaddon? () ; Old Camber workstation
  (string= system-name "abaddon"))
(defun corinth? () ; Samsung RV510 laptop (2012)
  (string= system-name "corinth"))
(defun ezekiel? () ; California
  (string= system-name "ezekiel"))
(defun habakkuk? () ; home
  (string= system-name "habakkuk"))
(defun naaman? () ; Atlanta
  (string= system-name "naaman"))
(defun nephesh? () ; MacBook Pro 15" (2014)
  (string= system-name "nephesh"))
(defun tcc-cgore? () ; Climate MacBook Pro 15" (2015)
  (or (string= system-name "tcc-cgore")
      (string= system-name "tcc-cgore.corp.climate.com")))

(server-start)

(setenv "PATH"
        (concat
         (expand-file-name "~/bin") ":"
         "/usr/local/bin:"
         "/usr/local/sbin:"
         (expand-file-name "~/.rvm/bin") ":"
         (expand-file-name "~/.rvm/sbin") ":"
         "/usr/texbin:"
         "/Library/TeX/texbin:"
         "/usr/bin:"
         "/usr/sbin:"
         "/bin:"
         "/sbin:"
         "/opt/pixie:"
         "/opt/sbin:"
         (getenv "PATH")))

(setq exec-path (append exec-path '("/usr/local/bin"
                                    "/usr/local/sbin"
                                    "/usr/texbin"
                                    (expand-file-name "~/.rvm/bin")
                                    (expand-file-name "~/.rvm/sbin")
                                    "/opt/pixie")))

(setq programming-mode-hooks-list
      '(c-mode-hook
          clojure-mode-hook
          c++-mode-hook
          coffee-mode-hook
          emacs-lisp-mode-hook
          html-mode-hook
          javascript-mode-hook
          lisp-mode-hook
          org-mode-hook
          puppet-mode-hook
          python-mode-hook
          ruby-mode-hook))

(let ((third-party "~/.emacs.d/third-party/"))
  (add-to-list 'load-path third-party)
  (mapcar #'(lambda (path)
              (add-to-list 'load-path (concat third-party path)))
          '("curry-gist"
            "emacs-soap-client"
            "rails-reloaded"
            "ruby-electric")))

(load "~/.emacs.d/utilities.el")

(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))
(setq url-http-attempt-keepalives nil)

(require 'ido)
(ido-mode t)

(defun dark-background ()
  (interactive)
  (load-theme 'reverse t))
(defun light-background ()
  (interactive)
  (disable-theme 'reverse))
(dark-background)

(defun fixed-buffer-width ()
  (cond ((not window-system) 78)
        ((abaddon?)          100)
        ((corinth?)          80)
        ((habakkuk?)         100)
        ((nephesh?)          100)
        (window-system       100)
        (t                   78)))

(when window-system
  (setq default-frame-alist `((width  . ,(fixed-buffer-width))
                              (height . 40))))

(setq-default c-basic-offset 4
              c-default-style "linux"
              fill-column 80) ; 80-wide for M-q.

(setq tab-width 2)

(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

(setq compilation-scroll-output t
      default-directory "~"
      indent-tabs-mode nil ; Don't mix tabs and spaces, that is stupid.

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
(when (not (darwin?))
  (display-time))
(column-number-mode 1)

(blink-cursor-mode 0)

(setenv "MANWIDTH" (number-to-string (fixed-buffer-width)))

;;; SLIME setup.
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
(let ((fp "/Users/cgore/quicklisp/slime-helper.el"))
  (when (file-exists-p fp)
    (load (expand-file-name fp))))
  ;; Replace "sbcl" with the path to your implementation
;;(setq inferior-lisp-program "sbcl")

(cond ((darwin?) (setq inferior-lisp-program "/usr/local/bin/sbcl"))
      ((linux?)  (setq inferior-lisp-program "/usr/bin/sbcl")))
(when (linux?)
  (add-to-list 'load-path "~/programming/lisp/slime/") ; My SLIME directory.
  (require 'slime-autoloads)
  (slime-setup))
(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("cd03a600a5f470994ba01dd3d1ff52d5809b59b4a37357fa94ca50a6f7f07473" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "ca229a0a89717c8a6fe5cd580ee2a85536fbafce6acb107d33cf38d52e2f492c" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (ein sass-mode rainbow-mode emamux echo-bell reverse-theme yasnippet yaml-mode tabulated-list starter-kit-eshell soothe-theme solarized-theme smex slime-annot seti-theme scss-mode s ruby-electric rspec-mode robe rainbow-identifiers rainbow-delimiters rainbow-blocks projectile pixie-mode php-mode paredit package+ neotree multi-term mc-extras markdown-mode magit json-mode ipython ido-ubiquitous idle-highlight-mode helm-w3m haml-mode fuzzy find-file-in-project espresso-theme elisp-slime-nav django-theme dirtree dired-rainbow color-theme coffee-mode clojure-test-mode afternoon-theme ack-and-a-half ace-jump-mode ac-slime)))
 '(safe-local-variable-values
   (quote
    ((encoding . utf-8)
     (whitespace-line-column . 80)
     (lexical-binding . t))))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(socks-server (quote ("default" "localhost" 9999 5)))
 '(term-bind-key-alist
   (quote
    (("C-c C-c" . term-interrupt-subjob)
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
     ("C-c C-j" . term-line-mode)
     ("C-c C-k" . term-char-mode)
     ("C-c C-e" . term-send-escape))))
 '(term-default-bg-color (face-background (quote default)))
 '(term-default-fg-color (face-foreground (quote default)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(woman-bold-headings t)
 '(woman-fill-column 100)
 '(woman-fill-frame t))

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

;; Limit the buffer for shells.
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 10000)

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

(defun bash ()
  (interactive)
  (let ((multi-term-program "bash")
        (multi-term-buffer-name "bash"))
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
(when (or (linux?) (darwin?))
  (require 'json-mode))
(setq js-indent-level 2)

;; Ack!
(require 'ack-and-a-half)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;;; Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t
      projectile-switch-project-action 'projectile-dired
      projectile-use-git-grep t
      projectile-project-root-files (quote ("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" ".git" ".projectile_root"))
      projectile-project-root-files-bottom-up (quote (".projectile" ".hg" ".fslckout" ".bzr" "_darcs"))
      projectile-file-exists-remote-cache-expire (* 10 60))


;;; Ruby
(require 'inf-ruby)
(require 'robe)
(require 'rspec-mode)
(require 'ruby-electric)
(require 'ruby-electric)
(require 'ruby-mode)
(require 'yari) ; ri interface
(require 'inf-ruby)
(require 'ruby-block)
(require 'rspec-mode)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle 'overlay)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(setq ruby-deep-arglist nil)
(setq ruby-deep-indent-paren nil)
(add-hook 'ruby-mode-hook 'robe-mode)
(setq rspec-use-rake-when-possible nil)

;;; Ruby on Rails
(require 'haml-mode)
(setq scss-compile-at-save nil)
(require 'sass-mode)
(require 'scss-mode)

(require 'tls)

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
      erc-log-write-after-insert t)

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

;;; Org Mode
(require 'org)
(require 'org-install)
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-log-done t)
(add-hook 'org-mode-hook 'org-indent-mode)

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
(global-set-key (kbd "<f2> l") 'magit-log-current)
(global-set-key (kbd "<f2> t") 'magit-log-all)
(setq magit-last-seen-setup-instructions "1.4.0")

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

(require 'eshell)
(setq eshell-path-env (concat "/usr/local/bin" ":"
                              eshell-path-env))

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

;;; Spell Checking
(mapcar #'(lambda (mode-hook)
            (add-hook mode-hook 'flyspell-mode))
        '(latex-mode-hook
          magit-log-edit-mode-hook
          org-mode-hook))
(mapcar #'(lambda (mode-hook)
            (add-hook mode-hook 'flyspell-prog-mode))
        programming-mode-hooks-list)

(mapcar #'(lambda (mode-hook)
            (add-hook mode-hook 'linum-mode)
            (add-hook mode-hook 'auto-complete-mode)
            (add-hook mode-hook 'rainbow-mode)
            (add-hook mode-hook 'rainbow-delimiters-mode)
            (add-hook 'html-mode-hook 'flyspell-prog-mode))
        programming-mode-hooks-list)

(mapcar #'(lambda (mode-hook)
            (add-hook mode-hook 'paredit-mode))
        '(clojure-mode-hook
          emacs-lisp-mode-hook
          eval-expression-minibuffer-setup-hook
          ielm-mode-hook
          lisp-mode-hook
          lisp-interaction-mode-hook
          slime-mode-hook
          slime-repl-mode-hook))

;;; Pixie
(add-hook 'pixie-mode-hook #'inf-clojure-minor-mode)

;;; Python
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


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



;;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; Clojure
(dolist (p '(auto-complete
             clojure-mode
             cider
             cider-test))
  (require p))
(global-set-key (kbd "C-c M-c") 'cider-connect)
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-repl-result-prefix ";; => ")
(setq cider-interactive-eval-result-prefix ";; -> ")
(setq cider-repl-history-size 10000)
(setq cider-repl-history-file (cond ((nephesh?)   "/Users/cgore/.emacs.d/cider-repl.history")
                                    ((tcc-cgore?) "/Users/chris.gore/.emacs.d/cider-repl.history")
                                    (t            "/home/chris/.emacs.d/cider-repl.history")))
(eval-after-load 'cider
  '(progn
     (add-hook 'clojure-mode-hook 'cider-mode)
     (add-hook 'cider-repl-mode-hook 'paredit-mode)
     (local-set-key (kbd "M-<return>") 'newline)))

(global-set-key (kbd "M-<return>") 'newline)

;;; YAML
(require 'yaml-mode)
(require 'yaml-mode)

;;; CoffeeScript
(require 'coffee-mode)
(setq coffee-tab-width 2)

;;; Ace Jump Mode
(require 'ace-jump-mode)
(global-set-key (kbd "C-'") 'ace-jump-mode)
(global-set-key (kbd "C-M-'") 'ace-jump-mode-pop-mark)

;;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c C-+") 'mc/mark-all-dwim)
(require 'mc-extras)
(define-key mc/keymap (kbd "C-. C-d") 'mc/remove-current-cursor)
(define-key mc/keymap (kbd "C-. d")   'mc/remove-duplicated-cursors)
(define-key mc/keymap (kbd "C-. =")   'mc/compare-chars)
;;(define-key cua--rectangle-keymap (kbd "C-. C-,") 'mc/cua-rectangle-to-multiple-cursors)
(mc/cua-rectangle-setup)


;; Autocomplete config
(define-key ac-completing-map [return] nil) ; no enter (1.)
(define-key ac-completing-map "\r" nil) ; no enter (2.)
(define-key ac-completing-map "\t" 'ac-complete) ; use tab to complete
(put 'upcase-region 'disabled nil)

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))


;; Neotree - a nice directory listing panel on the left side of your Emacs.
(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)

(defun default-font-and-theme ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 120 :width normal :family "Monaco")))))
  (dark-background))

(default-font-and-theme)

(defun present-to-everybody ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 260 :width normal :family "Monaco")))))
  (light-background))

(defun google-hangouts-sucks ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 180 :width normal :family "Monaco")))))
  (light-background))

(require 'echo-bell)
(echo-bell-mode)

(when (tcc-cgore?)
  (load "~/.emacs.d/climate.el"))
