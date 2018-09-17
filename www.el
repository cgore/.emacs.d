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

(when (darwin?)
  (setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
(defun browse-url-at-point-chrome ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-chrome))
    (browse-url-at-point)))
(global-set-key (kbd "<f6> c") 'browse-url-at-point-chrome)
