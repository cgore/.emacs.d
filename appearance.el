;;; Line Numbers
(unless window-system ; Only when in non-GUI
  (setq linum-format "%4d  ")) ; Add spacing to the right of line numbers

(beacon-mode 1)

(defun reset-line-number-heights ()
  "Reset line number face heights to match the default face.
Useful for themes where large Org headlines bleed into the line numbers."
  (interactive)
  (let ((height (face-attribute 'default :height)))
    ;; Main line number face
    (set-face-attribute 'line-number nil :height height)
    ;; Current line (the highlighted one)
    (set-face-attribute 'line-number-current-line nil :height height)
    ;; Optional: relative line number faces (if you use relative numbering)
    ;; (set-face-attribute 'line-number-major-tick nil :height height)
    ;; (set-face-attribute 'line-number-minor-tick nil :height height)
    ))

(defun dark-background ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-night t)
  ;; (load-theme 'doom-laserwave t)
  ;; (load-theme 'doom-outrun-electric t)
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

(defun organic-green-theme ()
  (interactive)
  (load-theme 'organic-green t)
  (reset-term-colors))

(defun default-theme ()
  (light-background))

(defun set-fira-code-font ()
  "Cf. <https://github.com/tonsky/FiraCode>

OSX:

brew tap homebrew/cask-fonts
brew install --cask font-fira-code

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
                           :height 160 :width normal :family "Fira Code")))))
  (default-theme))

(default-font-and-theme)

(defun slightly-bigger-font ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 180 :width normal :family "Fira Code")))))
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
                           :height 80 :width normal :family "Fira Code")))))
  (default-theme))
