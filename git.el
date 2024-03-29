(add-to-list 'auto-mode-alist '("\\.gitconfig" . conf-mode))
(global-set-key (kbd "<f2> <f2>") 'magit-status)
(global-set-key (kbd "<f2> g")    'magit-status)
(global-set-key (kbd "<f2> l")    'magit-log-current)
(global-set-key (kbd "<f2> t")    'magit-log-all)
(global-set-key (kbd "<f2> !")    'revert-buffer)
(setq magit-last-seen-setup-instructions "1.4.0")

;; https://github.com/magit/with-editor
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)

(defun git-pull-rebase-master ()
  (interactive)
  (shell-command "git stash")
  (shell-command "git checkout master")
  (shell-command "git pull --rebase"))
