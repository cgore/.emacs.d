;;; -*- lexical-binding: t; -*-

(when (alamoth?)
  (setq gptel-backend
        (gptel-make-openai "habakkuk.cgore.com"
          :host "10.0.2.2:1234"
          :protocol "http"
          :endpoint "/v1/chat/completions"
          :stream t
          :key "not-needed-no-key"
          :models '("qwen3-coder-30b-a3b-instruct"))))

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

(with-eval-after-load 'gptel-autocomplete
  (define-key gptel-autocomplete-completion-map (kbd "C-e") #'gptel-accept-completion)
  (define-key gptel-autocomplete-completion-map (kbd "TAB") #'gptel-accept-completion))
