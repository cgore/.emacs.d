(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun new-notes ()
  (interactive)
  (let ((newbuf (generate-new-buffer
                 (format-time-string "Untitled Notes - %a %-m/%d/%Y %-I:%M:%S %p.md"
                                     (current-time)))))
    (switch-to-buffer newbuf)
    (setq buffer-offer-save t)
    (markdown-mode)
    newbuf))
(global-set-key (kbd "<f8> n") 'new-notes)
