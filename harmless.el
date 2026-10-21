;;; -*- lexical-binding: t; -*-

;; Local checkout of Harmless (Emacs-native AI harness).
;; On Windows, ~ is %APPDATA%, so ~/programming is not D:/Programming.
(dolist (dir (list (expand-file-name "~/programming/emacs/harmless/lisp")
                   "d:/Programming/Emacs/harmless/lisp"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))
(require 'harmless)

(setq harmless-providers (list (harmless-make-xai "xAI")
                               (harmless-make-anthropic "Anthropic")
                               (harmless-make-openai))
      harmless-default-provider-name "xAI"
      harmless-default-model "grok-4.7"
      harmless-default-reasoning-effort "xhigh")

;; Browser login: M-x harmless-login, then pick the connection.
;; Default names keep existing token files (auth.json, auth-anthropic.json).
;; Then: M-x harmless
