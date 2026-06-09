;;; -*- lexical-binding: t; -*-

(when (alamoth?)
  (setq gptel-backend
        (gptel-make-openai "habakkuk.cgore.com"
          :host "10.0.2.2:1234"
          :protocol "http"
          :endpoint "/v1/chat/completions"
          :stream t
          :key "not-needed-no-key"
          :models '("google/gemma-4-31b"
                    ;;"google/gemma-4-26b-a4b"
                    ))))

