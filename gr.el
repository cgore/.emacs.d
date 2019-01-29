(defun gri-dev ()
  (interactive)
  (setenv "POLARIS_ENV" "dev")
  (setenv "LIPS_ENV" "dev")
  (setenv "RA_ENV" "dev"))

(defun gri-production ()
  (interactive)
  (setenv "POLARIS_ENV" "production")
  (setenv "LIPS_ENV" "production")
  (setenv "RA_ENV" "dev"))

(defun eshell/jira (ticket)
  "Go to the directory for notes on the specific jira TICKET."
  (let ((ticket-directory (concat "/Users/cgore/gr/tickets/" (upcase ticket))))
    (when (not (file-exists-p ticket-directory))
      (make-directory ticket-directory))
    (cd ticket-directory)))

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

(defun eshell/field-report ()
  (eshell/grcode "field-report"))

(defun eshell/lip-service ()
  (eshell/grcode "lip-service"))

(defun eshell/ns-toolkit ()
  (eshell/grcode "ns-toolkit"))

(defun eshell/red-arrow ()
  (eshell/grcode "red-arrow"))

(defun eshell/rockford ()
  (eshell/grcode "rockford"))

(defun eshell/sulley ()
  (eshell/grcode "sulley"))

(defun eshell/underdog ()
  (eshell/grcode "underdog"))

(defun eshell/verification-api ()
  (eshell/grcode "verification-api"))
