;;;; Copyright © 2013-2017, Christopher Mark Gore,
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

(prefer-coding-system 'utf-8)

(package-initialize)

(load "~/.emacs.d/init/packages.el")
(load "~/.emacs.d/init/boxen.el")

(server-start)

(when window-system
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
           (getenv "PATH"))))

(setq exec-path (append exec-path '("/usr/local/bin"
                                    "/usr/local/sbin"
                                    "/usr/texbin"
                                    (expand-file-name "~/.rvm/bin")
                                    (expand-file-name "~/.rvm/sbin")
                                    "/opt/pixie")))

(setq programming-mode-hooks-list
      '(sh-mode-hook
        c-mode-hook
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

(global-flycheck-mode)

(let ((third-party "~/.emacs.d/third-party/"))
  (add-to-list 'load-path third-party)
  (mapcar #'(lambda (path)
              (add-to-list 'load-path (concat third-party path)))
          '("curry-gist"
            "emacs-soap-client"
            "rails-reloaded"
            "ruby-electric")))

(load "~/.emacs.d/utilities.el")

(require 'ido)
(ido-mode t)

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
      visible-bell t
      ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(put 'scroll-left 'disabled nil)

;;; Toolbar setup
(when (not (darwin?))
  (display-time))
(column-number-mode 1)

(blink-cursor-mode 0)

(setenv "MANWIDTH" (number-to-string (fixed-buffer-width)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/Users/chris.gore/anaconda")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-day)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "cd03a600a5f470994ba01dd3d1ff52d5809b59b4a37357fa94ca50a6f7f07473" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "ca229a0a89717c8a6fe5cd580ee2a85536fbafce6acb107d33cf38d52e2f492c" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (restclient conda color-theme-sanityinc-tomorrow leuven-theme highlight-indentation ensime go-mode shen-lisp sly flycheck-pos-tip flycheck-clojure flycheck clojure-cheatsheet kibit-helper inf-clojure ac-cider paper-theme eruby-mode auto-highlight-symbol magit-gitflow erlang w3m markdown-preview-mode puppet-mode python-mode ein sass-mode rainbow-mode emamux echo-bell reverse-theme yasnippet yaml-mode tabulated-list starter-kit-eshell soothe-theme solarized-theme smex slime-annot seti-theme scss-mode s ruby-electric rspec-mode robe rainbow-identifiers rainbow-delimiters rainbow-blocks projectile pixie-mode php-mode paredit package+ neotree multi-term mc-extras markdown-mode magit json-mode ipython ido-ubiquitous idle-highlight-mode helm-w3m haml-mode fuzzy find-file-in-project espresso-theme elisp-slime-nav django-theme dirtree dired-rainbow color-theme coffee-mode clojure-test-mode afternoon-theme ack-and-a-half ace-jump-mode ac-slime)))
 '(py-shell-name "/usr/local/bin/python")
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

(load "~/.emacs.d/init/multi-term.el")

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


(require 'tls)

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

(load "~/.emacs.d/init/eshell.el")

;;; Line Numbers
(unless window-system ; Only when in non-GUI
  (setq linum-format "%4d  ")) ; Add spacing to the right of line numbers

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
            (add-hook mode-hook 'auto-highlight-symbol-mode)
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

(setq inferior-lisp-program "/usr/bin/env sbcl --noinform --no-linedit")

;;; Pixie
(add-hook 'pixie-mode-hook #'inf-clojure-minor-mode)

(load "~/.emacs.d/init/clojure.el")
(load "~/.emacs.d/init/erc.el")
(load "~/.emacs.d/init/git.el")
(load "~/.emacs.d/init/maxima.el")
(load "~/.emacs.d/init/mc.el")
(load "~/.emacs.d/init/python.el")
(load "~/.emacs.d/init/ruby.el")

;;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; YAML
(require 'yaml-mode)
(add-hook 'yaml-mode-hook 'linum-mode)

;;; CoffeeScript
(require 'coffee-mode)
(setq coffee-tab-width 2)

;;; Ace Jump Mode
(require 'ace-jump-mode)
(global-set-key (kbd "C-'") 'ace-jump-mode)
(global-set-key (kbd "C-M-'") 'ace-jump-mode-pop-mark)

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
(global-set-key (kbd "<f8> <f8>") 'neotree-toggle)
(global-set-key (kbd "<f8> /") 'neotree-dir)
(global-set-key (kbd "<f8> .") 'neotree-find)

(require 'echo-bell)
(echo-bell-mode)

(load "~/.emacs.d/appearance.el")

(when (tcc-cgore?)
  (load "~/.emacs.d/local/climate.el"))

(require 'restclient) ; https://github.com/pashky/restclient.el

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "Fira Code")))))
