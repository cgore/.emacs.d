(setq gptel-backend
      (gptel-make-openai "parbar.cgore.com"
        :host "192.168.50.40:1234"
        :protocol "http"
        :endpoint "/v1/chat/completions"
        :stream t
        :key "not-needed-no-key"
        :models '("qwen/qwen3.5-9b")))
