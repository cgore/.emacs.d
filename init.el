(prefer-coding-system 'utf-8)

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/boxen.el")

(server-start)

;; /usr/local/opt/openjdk/bin:
;; /Users/cgore/.nvm/versions/node/v10.13.0/bin:
;; /usr/local/opt/ncurses/bin:
;; /usr/local/opt/gettext/bin:

(setenv "PAGER" "cat")
;;(setenv "N_PREFIX" (expand-file-name "~/n"))
(setenv "NVM_DIR" (expand-file-name "~/.nvm"))
(setq exec-path
      (append `(,(expand-file-name "~/bin")
                ,(expand-file-name "~/.cargo/bin")
                ;; ,(expand-file-name "~/n/bin")
                ,(expand-file-name "~/.nvm/versions/node/v10.13.0/bin/node")
                "/usr/local/share/dotnet"
                ,(expand-file-name "~/.dotnet/tools")
                "/usr/local/bin"
                "/usr/local/sbin"
                "/usr/bin"
                "/usr/sbin"
                "/bin"
                "/sbin")
              exec-path))

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
(load "~/.emacs.d/common-lisp.el")

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
 '(custom-enabled-themes '(sanityinc-tomorrow-day))
 '(custom-safe-themes
   '("730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "6231254e74298a1cf8a5fee7ca64352943de4b495e615c449e9bb27e2ccae709" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "d2bd16a8bcf295dce0b70e1d2b5c17bb34cb28224a86ee770d56e6c22a565013" "3f67aee8f8d8eedad7f547a346803be4cc47c420602e19d88bdcccc66dba033b" "81b6536ffa1466fa00f9b8dcd14c0995ef15d595ab903572bba484b6be3eacaa" "87d46d0ad89557c616d04bef34afd191234992c4eb955ff3c60c6aa3afc2e5cc" "50ff65ab3c92ce4758cc6cd10ebb3d6150a0e2da15b751d7fbee3d68bba35a94" "abdb1863bc138f43c29ddb84f614b14e3819982936c43b974224641b0b6b8ba4" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "73ad471d5ae9355a7fa28675014ae45a0589c14492f52c32a4e9b393fcc333fd" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "50b64810ed1c36dfb72d74a61ae08e5869edc554102f20e078b21f84209c08d1" "bbb521edff9940ba05aeeb49f9b247e95e1cb03bd78de18122f13500bda6514f" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "44961a9303c92926740fc4121829c32abca38ba3a91897a4eab2aa3b7634bed4" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "f984e2f9765a69f7394527b44eaa28052ff3664a505f9ec9c60c088ca4e9fc0b" "9c4acf7b5801f25501f0db26ac3eee3dc263ed51afd01f9dcfda706a15234733" "722e1cd0dad601ec6567c32520126e42a8031cd72e05d2221ff511b58545b108" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "ef403aa0588ca64e05269a7a5df03a5259a00303ef6dfbd2519a9b81e4bce95c" "d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "b67b2279fa90e4098aa126d8356931c7a76921001ddff0a8d4a0541080dee5f6" "428bdd4b98d4d58cd094e7e074c4a82151ad4a77b9c9e30d75c56dc5a07f26c5" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "b0c5c6cc59d530d3f6fbcfa67801993669ce062dda1435014f74cafac7d86246" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "1025e775a6d93981454680ddef169b6c51cc14cea8cb02d1872f9d3ce7a1da66" "808b47c5c5583b5e439d8532da736b5e6b0552f6e89f8dafaab5631aace601dd" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "cd03a600a5f470994ba01dd3d1ff52d5809b59b4a37357fa94ca50a6f7f07473" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "ca229a0a89717c8a6fe5cd580ee2a85536fbafce6acb107d33cf38d52e2f492c" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(package-selected-packages
   '(tide typescript-mode slime ac-cider ac-slime ace-jump-mode ack-and-a-half afternoon-theme aggressive-indent all-the-icons all-the-icons-dired auto-complete auto-highlight-symbol base16-theme beacon cloc clojure-mode clojure-test-mode coffee-mode color-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow color-theme-solarized conda csharp-mode cyberpunk-theme dash dash-docs dash-functional dired-rainbow dirtree django-theme doom-themes ein elisp-slime-nav elpy emamux ensime erlang eshell-autojump espresso-theme ewal-doom-themes fennel-mode find-file-in-project flycheck flycheck-clojure flycheck-pos-tip flymake-go flymd fuzzy geiser go-mode gorepl-mode green-is-the-new-black-theme haml-mode helm-w3m highlight-indentation ht idle-highlight-mode ido-ubiquitous indium inf-clojure inf-ruby ipython jq-format jq-mode js2-refactor json-mode kibit-helper kotlin-mode kubernetes leuven-theme logview lua-mode magit markdown-mode markdown-preview-mode mc-extras minimap multi-term multiple-cursors neotree nodejs-repl ob-restclient omnisharp package+ paper-theme paredit php-mode pixie-mode plantuml-mode popup-complete projectile puppet-mode python-mode queue rainbow-blocks rainbow-delimiters rainbow-identifiers rainbow-mode reverse-theme rjsx-mode robe rspec-mode ruby-electric rust-mode s sass-mode scss-mode seti-theme shen-lisp slime-annot smex solarized-theme solidity-mode soothe-theme starter-kit-eshell string-inflection tabulated-list terraform-doc terraform-mode uuidgen w3m which-key wsd-mode xref-js2 yafolding yaml-mode))
 '(py-shell-name "/usr/local/bin/python")
 '(safe-local-variable-values
   '((encoding . utf-8)
     (whitespace-line-column . 80)
     (lexical-binding . t)))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(socks-server '("default" "localhost" 9999 5))
 '(term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob)
     ("C-p" . previous-line)
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
     ("C-c C-e" . term-send-escape)))
 '(term-default-bg-color (face-background 'default))
 '(term-default-fg-color (face-foreground 'default))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
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
(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))


(load "~/.emacs.d/www.el")

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
(load "~/.emacs.d/folding.el")
(load "~/.emacs.d/pixie.el")
(load "~/.emacs.d/clojure.el")
;; (load "~/.emacs.d/clojurescript.el")
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
(load "~/.emacs.d/typescript.el")

(setq programming-mode-hooks-list
      '(sh-mode-hook
        c-mode-hook
        c++-mode-hook
        csharp-mode-hook
        clojure-mode-hook
        coffee-mode-hook
        emacs-lisp-mode-hook
        go-mode-hook
        html-mode-hook
        javascript-mode-hook
        js2-mode-hook
        json-mode-hook
        kotlin-mode-hook
        lisp-mode-hook
        org-mode-hook
        puppet-mode-hook
        python-mode-hook
        ruby-mode-hook
        rust-mode-hook
        rjsx-mode-hook
        solidity-mode-hook
        typescript-mode-hook))

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
                      whitespace-mode
                      yafolding-mode)))
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
          ;;slime-mode-hook
          ;;slime-repl-mode-hook
          ))


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
    (shell-command-on-region
     (mark) (point)
     "python -m json.tool"
     (buffer-name) t)))

(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region
     (mark) (point)
     "xmllint --format --encode utf-8 -"
     (buffer-name) t)))

(load "~/.emacs.d/appearance.el")

(load "~/.emacs.d/windows.el")
(load "~/.emacs.d/neotree.el")
(load "~/.emacs.d/string-inflection.el")

(require 'restclient) ; https://github.com/pashky/restclient.el

(when (gr-cgore?)
  (load "~/.emacs.d/gr.el")
  (load "~/.emacs.d/secrets/gr.el")
  (setq exec-path
        (append exec-path `(,(expand-file-name "~/gr/code/kubeclj")
                            ,(expand-file-name "~/n/bin")
                            "/usr/local/opt/mongodb-community@3.6/bin"))))

(when window-system
  (setenv "PATH"
          (concat (string-join exec-path ":")
                  ":" (getenv "PATH"))))

(load "~/.emacs.d/aws.el")

(load "~/.emacs.d/keys.el")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "Fira Code"))))
 '(whitespace-tab ((t (:foreground "#636363")))))
(put 'erase-buffer 'disabled nil)


(defun nuke-env (variable)
  (interactive)
  )

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq default-buffer-file-coding-system 'utf-8-unix)
