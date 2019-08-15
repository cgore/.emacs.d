(defun s3-ls (s3-uri)
  "List S3 bucket entries.
S3-URI are formatted lie like: s3://some-bucket/some/prefix"
  (interactive "sS3 URI (s3://some-bucket/some/prefix)? ")
  (shell-command (format "aws s3 ls %s --recursive --human-readable --summarize"
                         s3-uri)))

(defun ssm-decrypt (ssm-parameter-name)
  "Retrieve and decrypt a single SSM parameter.
SSM-PARAMETER-NAME is formatted like: /foo/bar/baz/someSecretToken"
  (interactive "sSSM parameter? ")
  (shell-command (format "aws ssm get-parameter --name %s --with-decryption"
                         ssm-parameter-name)))

(defun ssm-decrypt-recursive (ssm-path)
  "Retrieve and decrypt all SSM parameters under a path, recursively.
SSM-PATH is formatted like: /foo/bar/baz
And that would retrieve and decrypt /foo/bar/baz/someSecretToken,
                                    /foo/bar/baz/someOtherSecretToken
                                    ..."
  (interactive "sSSM path? ")
  (shell-command (format "aws ssm get-parameters-by-path --path %s --recursive --with-decryption"
                         ssm-path)))

(defun ssm-put-secure-string! (ssm-path new-value)
  (interactive "sSSM path? \nsValue? ")
  (shell-command (format "aws ssm put-parameter --name %s --value %s --type SecureString --overwrite"
                         ssm-path new-value)))
