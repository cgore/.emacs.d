(defsubst icurry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more)
      (interactive)
      (apply function (append arguments more)))))

;;; color these functions like keywords
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(icurry\\)[ \t\n\r]" 1 font-lock-keyword-face)))

;; Rounding numbers
(defun get-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789.-")
  (or (looking-at "[0123456789.-]+")
      (error "No number at point"))
  (string-to-number (match-string 0)))

(defun round-number-at-point-to-decimals (decimal-count)
  (interactive "NDecimal count: ")
  (let ((mult (expt 10 decimal-count)))
    (replace-match (number-to-string
                    (/ (fround
                        (* mult
                           (get-number-at-point)))
                       mult)))))

(defun mark-entire-word ()
  "Mark the entire word (with superword mode!) under point."
  (interactive)
  (superword-mode 1)
  (backward-word)
  (mark-word)
  ;(kill-ring-save (region-beginning) (region-end))
  (superword-mode -1))

(defun cleanup-environment-variables ()
  "Completely remove any environment variables that no longer have a value."
  (interactive)
  (setq process-environment
        (-filter (lambda (env-def)
                   (string-match "=" env-def))
                 process-environment)))

(defun add-to-ldflags (flag)
  "Adds another flag to the LDFLAGS environment variable."
  (setenv "LDFLAGS" (s-trim (s-join " " (list (getenv "LDFLAGS") flag)))))

(defun add-to-cppflags (flag)
  "Adds another flag to the CPPFLAGS environment variable."
  (setenv "CPPFLAGS" (s-trim (s-join " " (list (getenv "CPPFLAGS") flag)))))
