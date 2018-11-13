(defun gri-dev ()
  (interactive)
  (setenv "POLARIS_ENV" "dev")
  (setenv "LIPS_ENV" "dev"))

(defun gri-production ()
  (interactive)
  (setenv "POLARIS_ENV" "production")
  (setenv "LIPS_ENV" "production"))

(defun eshell/jira (ticket)
  "Go to the directory for notes on the specific jira TICKET."
  (let ((ticket-directory (concat "/Users/cgore/gr/tickets/" ticket)))
    (when (not (file-exists-p ticket-directory))
      (make-directory ticket-directory))
    (cd ticket-directory)))

(defun eshell/grcode (repo)
  (cd (concat "/Users/cgore/gr/code/" repo)))

(defun eshell/field-report ()
  (eshell/grcode "field-report"))

(defun eshell/lip-service ()
  (eshell/grcode "lip-service"))

(defun eshell/ns-toolkit ()
  (eshell/grcode "ns-toolkit"))

(defun eshell/red-arrow ()
  (eshell/grcode "red-arrow"))

(defun eshell/sulley ()
  (eshell/grcode "sulley"))

(defun eshell/underdog ()
  (eshell/grcode "underdog"))
