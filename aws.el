(defun ssm-decrypt (ssm-path)
  (interactive "sSSM path? ")
  (shell-command (format "aws ssm get-parameters-by-path --path %s --recursive --with-decryption"
                         ssm-path)))
