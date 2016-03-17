(defun envmgr (environment)
  (concat "/Users/chris.gore/climate/code/devops/utilities/target/python/bin/envmgr -e " environment))

(defun climate-envmgr (environment)
  (mapcar (lambda (line)
            (let ((k=v (second (split-string line))))
              (when (stringp k=v)
                (let* ((k-v (split-string k=v "="))
                       (k (first k-v))
                       (v (second k-v)))
                  (setenv k v)))))
          (split-string (shell-command-to-string (envmgr environment))
                        "\n")))

(defun climate-ci ()
  (interactive)
  (climate-envmgr "ci"))

(defun climate-qa1 ()
  (interactive)
  (climate-envmgr "qa1"))

(defun climate-int ()
  (interactive)
  (climate-envmgr "integration"))

(defun climate-staging ()
  (interactive)
  (climate-envmgr "staging"))

(defun climate-prod ()
  (interactive)
  (climate-envmgr "production"))

(climate-qa1)
