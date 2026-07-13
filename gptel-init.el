;;; -*- lexical-binding: t; -*-

(when (alamoth?)
  (setq gptel-backend
        (gptel-make-openai "habakkuk.cgore.com"
          :host "10.0.2.2:1234"
          :protocol "http"
          :endpoint "/v1/chat/completions"
          :stream t
          :key "not-needed-no-key"
          :models '(
                    "ornith-1.0-35b"
                    ;; "google/gemma-4-31b-qat"
                    ;; "qwen3-coder-30b-a3b-instruct"
                    ))))

(use-package quelpa :ensure t)
(use-package quelpa-use-package :ensure t)
(use-package gptel-autocomplete
             :quelpa (gptel-autocomplete :fetcher github :repo "JDNdeveloper/gptel-autocomplete")
             :after gptel
             :config
             (gptel-autocomplete-mode 1))

(setq gptel-autocomplete-idle-delay 0.25)   ; faster trigger
(setq gptel-autocomplete-max-tokens 150)    ; keep responses snappy
(setq gptel-autocomplete-temperature 0.1)   ; more deterministic for code
(setq gptel-use-tools t)
(setq gptel-confirm-tool-calls 'auto)       ; or t for safety while testing
(setq gptel-include-tool-results t)
(setq gptel-max-tokens 16384)               ; or higher for agent loops

(with-eval-after-load 'gptel-autocomplete
  (define-key gptel-autocomplete-completion-map (kbd "C-e") #'gptel-accept-completion)
  (define-key gptel-autocomplete-completion-map (kbd "TAB") #'gptel-accept-completion))

;; Safety first — highly recommended while testing
(setq gptel-confirm-tool-calls t)        ; Ask before running any tool
(setq gptel-include-tool-results t)      ; Keep tool output in context

;; Quick access to the tools menu
(with-eval-after-load 'gptel
  (define-key gptel-mode-map (kbd "C-c t") #'gptel-tools))
