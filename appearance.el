;;; Line Numbers
(unless window-system ; Only when in non-GUI
  (setq linum-format "%4d  ")) ; Add spacing to the right of line numbers

(defun dark-background ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-eighties t)
  (reset-term-colors))

(defun light-background ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-day t)
  (reset-term-colors))

(defun blue-background ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-blue t)
  (reset-term-colors))

(defun set-fira-code-font ()
  "Cf. <https://github.com/tonsky/FiraCode>"
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
  (dark-background))

(default-font-and-theme)

(defun slightly-bigger-font ()
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 170 :width normal :family "Fira Code")))))
  (dark-background))

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
  (dark-background))

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
  (dark-background))
