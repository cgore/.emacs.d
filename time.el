(defun current-unix-timestamp ()
  "A simple function to figure out the current unix time stamp,
the number of seconds since their epoch."
  (interactive)
  (shell-command "date +'%s'"))
