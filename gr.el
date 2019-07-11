(defun eshell/jira (ticket)
  "Go to the directory for notes on the specific jira TICKET."
  (let ((ticket-directory (concat "/Users/cgore/gr/tickets/" (upcase ticket))))
    (when (not (file-exists-p ticket-directory))
      (make-directory ticket-directory))
    (cd ticket-directory)))


(defun eshell/bdv (ticket-number)
  (eshell/jira (format "BDV-%s" ticket-number)))

(defun eshell/grcode (repo)
  "Go to the directory for a Guaranteed Rate REPO."
  (let* ((code-directory "/Users/cgore/gr/code/")
         (repo-directory (concat code-directory repo)))
    (when (not (file-exists-p repo-directory))
      (eshell-printn (concat "Checking out a local copy of " repo " from github ..."))
      (cd code-directory)
      (shell-command (concat "git clone git@github.com:Guaranteed-Rate/" repo ".git")))
    (cd repo-directory)))

(defun eshell/call-me-maybe ()
  (eshell/grcode "call-me-maybe"))

(defun eshell/do-si-do ()
  (eshell/grcode "do-si-do"))
