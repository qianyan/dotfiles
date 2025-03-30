(use-package company
             :ensure t
             :defer t
             :config
             (global-company-mode)
             (setq company-idle-delay 1)
             (setq company-show-numbers t))

(use-package company-tabnine
  :requires (company)
  :ensure t)

(add-to-list 'company-backends #'company-tabnine)

;; install `llm` first
(use-package ellama
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "Chinese")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   ;; this model should be pulled to use it
	   ;; value should be the same as you print in terminal during pull
	   :chat-model "mistral:latest"
	   :embedding-model "mistral:latest"))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
	  '(("gemma:7b" . (make-llm-ollama
                           :chat-model "gemma:7b"
                           :embedding-model "gemma:7b"))
            ("gemma:2b" . (make-llm-ollama
			   :chat-model "gemma:2b"
			   :embedding-model "gemma:2b"))
            ("llama3" . (make-llm-ollama
			 :chat-model "llama3"
			 :embedding-model "llama3"))                      
            ("codellama" . (make-llm-ollama
			    :chat-model "codellama"
			    :embedding-model "codellama"))                      
	    ("mistral" . (make-llm-ollama
			  :chat-model "mistral:latest"
			  :embedding-model "mistral:latest"))
	    ("chinese" . (make-llm-ollama
			  :chat-model "llama2-chinese"
			  :embedding-model "llama2-chinese"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
	  (make-llm-ollama
	   :chat-model "mistral:latest"
	   :embedding-model "mistral:latest"))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm))

;;; `pass add api.deepseek.com` save into ~/.password_store/api.deepseek.com.gpg
(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-deepseek-key
    (lambda ()
      (auth-source-pass-get 'secret "api.deepseek.com")))))

(provide 'init-ai-assistant)
