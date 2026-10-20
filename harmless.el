;;; -*- lexical-binding: t; -*-

;; Local checkout of Harmless (Emacs-native AI harness).
(add-to-list 'load-path (expand-file-name "~/programming/emacs/harmless/lisp"))
(require 'harmless)

(setq harmless-providers (list (harmless-make-xai "xAI")
                               (harmless-make-xai "xAI work")
                               (harmless-make-anthropic "Anthropic")
                               (harmless-make-anthropic "Anthropic work")
                               (harmless-make-openai))
      harmless-default-provider-name "xAI"
      harmless-default-model "grok-4.6"
      harmless-default-reasoning-effort "xhigh")

;; Browser login: M-x harmless-login, then pick the connection.
;; Default names keep existing token files (auth.json, auth-anthropic.json).
;; Then: M-x harmless
