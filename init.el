(prefer-coding-system 'utf-8)

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/boxen.el")

(server-start)

(setenv "PAGER" "cat")

(setq exec-path
      (append `(,(expand-file-name "~/bin")
                ,(expand-file-name "~/n/bin")
                "/usr/local/bin"
                "/usr/local/sbin"
                "/usr/bin"
                "/usr/sbin"
                "/bin"
                "/sbin")
              exec-path))

(when (gr-cgore?)
  (load "~/.emacs.d/secrets/gr.el")
  (setq exec-path
        (append exec-path `(,(expand-file-name "~/gr/code/kubeclj")))))

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
        ruby-mode-hook
        rjsx-mode-hook))

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

(load "~/.emacs.d/c.el")

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
    ("5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "b67b2279fa90e4098aa126d8356931c7a76921001ddff0a8d4a0541080dee5f6" "428bdd4b98d4d58cd094e7e074c4a82151ad4a77b9c9e30d75c56dc5a07f26c5" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "b0c5c6cc59d530d3f6fbcfa67801993669ce062dda1435014f74cafac7d86246" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "1025e775a6d93981454680ddef169b6c51cc14cea8cb02d1872f9d3ce7a1da66" "808b47c5c5583b5e439d8532da736b5e6b0552f6e89f8dafaab5631aace601dd" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "cd03a600a5f470994ba01dd3d1ff52d5809b59b4a37357fa94ca50a6f7f07473" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "ca229a0a89717c8a6fe5cd580ee2a85536fbafce6acb107d33cf38d52e2f492c" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(package-selected-packages
   (quote
    (nodejs-repl xref-js2 js2-refactor rjsx-mode base16-theme geiser clj-refactor uuidgen eshell-autojump elpy logview gorepl-mode flymake-go minimap color-theme-solarized color-theme-sanityinc-solarized queue multiple-cursors inf-ruby clojure-mode cider auto-complete cloc restclient conda color-theme-sanityinc-tomorrow leuven-theme highlight-indentation ensime go-mode shen-lisp sly flycheck-pos-tip flycheck-clojure flycheck clojure-cheatsheet kibit-helper inf-clojure ac-cider paper-theme auto-highlight-symbol magit-gitflow erlang w3m markdown-preview-mode puppet-mode python-mode ein sass-mode rainbow-mode emamux reverse-theme yasnippet yaml-mode tabulated-list starter-kit-eshell soothe-theme solarized-theme smex slime-annot seti-theme scss-mode s ruby-electric rspec-mode robe rainbow-identifiers rainbow-delimiters rainbow-blocks projectile pixie-mode php-mode paredit package+ neotree multi-term mc-extras markdown-mode magit json-mode ipython ido-ubiquitous idle-highlight-mode helm-w3m haml-mode fuzzy find-file-in-project espresso-theme elisp-slime-nav django-theme dirtree dired-rainbow color-theme coffee-mode clojure-test-mode afternoon-theme ack-and-a-half ace-jump-mode ac-slime)))
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

(load "~/.emacs.d/multi-term.el")

(load "~/.emacs.d/javascript.el")

(load "~/.emacs.d/ack.el")

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

(load "~/.emacs.d/www.el")
(load "~/.emacs.d/thinking-bicycle.el")

;;; TeX and LaTeX
(add-to-list 'auto-mode-alist '("\\.latex$" . latex-mode))

;;; Buffer navigation.
(global-set-key (kbd "<XF86Back>") 'previous-buffer)
(global-set-key (kbd "<XF86Forward>") 'next-buffer)

(load "~/.emacs.d/noforn.el")

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

(load "~/.emacs.d/eshell.el")

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
            (mapcar #'(lambda (hook)
                       (add-hook mode-hook hook))
                    '(linum-mode
                      auto-complete-mode
                      rainbow-mode
                      rainbow-delimiters-mode
                      auto-highlight-symbol-mode
                      flyspell-prog-mode
                      whitespace-mode)))
        programming-mode-hooks-list)

(mapcar #'(lambda (mode-hook)
            (add-hook mode-hook 'paredit-mode))
        '(clojure-mode-hook
          emacs-lisp-mode-hook
          eval-expression-minibuffer-setup-hook
          ielm-mode-hook
          json-mode-hook
          lisp-mode-hook
          lisp-interaction-mode-hook
          slime-mode-hook
          slime-repl-mode-hook))

(setq inferior-lisp-program "/usr/bin/env sbcl --noinform --no-linedit")

(load "~/.emacs.d/pixie.el")
(load "~/.emacs.d/clojure.el")
(load "~/.emacs.d/erc.el")
(load "~/.emacs.d/git.el")
(load "~/.emacs.d/go.el")
(load "~/.emacs.d/maxima.el")
(load "~/.emacs.d/mc.el")
(load "~/.emacs.d/python.el")
(load "~/.emacs.d/ruby.el")
(load "~/.emacs.d/markdown.el")
(load "~/.emacs.d/coffeescript.el")
(load "~/.emacs.d/yaml.el")

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

(load "~/.emacs.d/appearance.el")

(load "~/.emacs.d/windows.el")
(load "~/.emacs.d/neotree.el")

(require 'restclient) ; https://github.com/pashky/restclient.el

(when window-system
  (setenv "PATH"
          (concat (string-join exec-path ":")
                  ":" (getenv "PATH"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "Fira Code")))))
