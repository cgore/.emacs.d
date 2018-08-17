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
