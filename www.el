;;; -*- lexical-binding: t; -*-

;; W3M
;; OSX:
;; $ brew install w3m

(setq browse-url-browser-function 'w3m-browse-url)

(global-set-key (kbd "<f6> w") 'browse-url-at-point)

(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-use-cookies t)

(when (darwin?)
  (setq browse-url-firefox-program "/usr/bin/open")
  (setq browse-url-firefox-arguments '("-a" "Firefox")))
(defun browse-url-at-point-firefox ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url-at-point)))
(global-set-key (kbd "<f6> f") 'browse-url-at-point-firefox)

(when (darwin?)
  (setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
(defun browse-url-at-point-chrome ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-chrome))
    (browse-url-at-point)))
(global-set-key (kbd "<f6> c") 'browse-url-at-point-chrome)


(defun browse-url-safari (url &optional _new-window)
  "Open URL in Safari on macOS.
The optional NEW-WINDOW argument is ignored."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let ((process-environment (browse-url-process-environment)))
    (start-process (concat "safari " url) nil
                   "open" "-a" "Safari" url)))
(function-put 'browse-url-safari 'browse-url-browser-kind 'external)

(when (darwin?)
  (setq browse-url-safari-program "/Applications/Safari.app/Contents/MacOS/Safari"))
(defun browse-url-at-point-safari ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-safari))
    (browse-url-at-point)))
(global-set-key (kbd "<f6> s") 'browse-url-at-point-safari)
