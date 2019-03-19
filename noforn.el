;; You can toggle back and forth with normal by C-\

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
(global-set-key (kbd "<f9> -<") "←")
(global-set-key (kbd "<f9> -->") "⟶")
(global-set-key (kbd "<f9> --<") "⟵")
(global-set-key (kbd "<f9> =>") "⇒")
(global-set-key (kbd "<f9> <=") "⇐")
(global-set-key (kbd "<f9> inf") "∞")
(global-set-key (kbd "<f9> :)") "☻")
