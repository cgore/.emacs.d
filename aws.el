(defun ssm-decrypt (ssm-parameter-name)
  (interactive "sSSM path? ")
  (shell-command (format "aws ssm get-parameter --name %s --with-decryption"
                         ssm-parameter-name)))

(defun ssm-decrypt-recursive (ssm-path)
  (interactive "sSSM path? ")
  (shell-command (format "aws ssm get-parameters-by-path --path %s --recursive --with-decryption"
                         ssm-path)))
