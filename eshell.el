(require 'eshell)
(setq eshell-path-env (concat "/usr/local/bin" ":"
                              eshell-path-env))


;; Taken from <http://www.emacswiki.org/emacs/EshellFunctions>
(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun format-commands (&rest commands)
  (mapcar (lambda (command)
            (shell-command (apply 'format command)))
          commands))

(defun eshell/git-branch-here (branch)
  (format-commands `("git push origin :%s" ,branch)
                   `("git branch -d %s" ,branch)
                   `("git checkout -b %s" ,branch)
                   `("git push origin %s:%s" ,branch ,branch)))

(defun eshell/git-experimental-here ()
  (eshell/git-branch-here "experimental"))

(global-set-key (kbd "<f9> <f9>") 'eshell)

(eval-after-load 'eshell
  '(require 'eshell-autojump nil t))

(setq eshell-last-dir-ring-size 500)

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize (format-time-string "%-m/%d/%Y %a %-I:%M:%S %p " (current-time))
                     'face `(:foreground "#aaaaff"))
         (abbreviate-file-name (eshell/pwd))
	       (if (= (user-uid) 0) " # " " $ "))))
