;;; Line Numbers
(unless window-system ; Only when in non-GUI
  (setq linum-format "%4d  ")) ; Add spacing to the right of line numbers

(beacon-mode 1)

(defun reset-linum-heights ()
  (interactive)
  ;; If you use nlinum or linum in org-mode, the larger headline sizes in some
  ;; themes could bleed into the line numbers.  Fix this by setting :height
  ;; explicitly for your line number plugins, after you've loaded the
  ;; theme. e.g.
  (let ((height (face-attribute 'default :height)))
    ;; for all linum/nlinum users
    (set-face-attribute 'linum nil :height height)
    ;; only for `linum-relative' users:
    ;;(set-face-attribute 'linum-relative-current-face nil :height height)
    ;; only for `nlinum-relative' users:
    ;;(set-face-attribute 'nlinum-relative-current-face nil :height height)
    ))

(defun dark-background ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-night t)
  ;; (load-theme 'doom-laserwave t)
  ;; (load-theme 'doom-outrun-electric t)
  ;; (reset-linum-heights)
  (reset-term-colors))

(defun light-background ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-day t)
  (reset-term-colors))

(defun blue-background ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-blue t)
  (reset-term-colors))

(defun green-background ()
  (interactive)
  (load-theme 'base16-greenscreen t)
  (reset-term-colors))

(defun default-theme ()
  (dark-background))

(defun set-fira-code-font ()
  "Cf. <https://github.com/tonsky/FiraCode>

OSX:

brew tap homebrew/cask-fonts
brew cask install font-fira-code

Ubuntu:

sudo apt-get install fonts-firacode

"
  (interactive)
  (when (window-system)
    (set-default-font "Fira Code"))
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

;;(set-fira-code-font)

(defun default-font-and-theme ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 140 :width normal :family "Fira Code")))))
  (default-theme))

(default-font-and-theme)

(defun slightly-bigger-font ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 170 :width normal :family "Fira Code")))))
  (default-theme))

(defun slightly-smaller-font ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 120 :width normal :family "Fira Code")))))
  (default-theme))

(defun present-to-everybody ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 300 :width normal :family "Fira Code")))))
  (light-background))

(defun msdos2k ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 440 :width normal :family "Fira Code")))))
  (default-theme))

(defun google-hangouts-sucks ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 220 :width normal :family "Fira Code")))))
  (light-background))

(defun google-meet-sucks ()
  (interactive)
  (google-hangouts-sucks))

(defun antman-uses-emacs ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 90 :width normal :family "Fira Code")))))
  (default-theme))
