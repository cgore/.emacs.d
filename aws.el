(defun s3-ls (s3-uri)
  (interactive "sS3 URI (s3://some-bucket/some/prefix)? ")
  (shell-command (format "aws s3 ls %s --recursive --human-readable --summarize"
                         s3-uri)))

(defun ssm-decrypt (ssm-parameter-name)
  (interactive "sSSM parameter? ")
  (shell-command (format "aws ssm get-parameter --name %s --with-decryption"
                         ssm-parameter-name)))

(defun ssm-decrypt-recursive (ssm-path)
  (interactive "sSSM path? ")
  (shell-command (format "aws ssm get-parameters-by-path --path %s --recursive --with-decryption"
                         ssm-path)))
