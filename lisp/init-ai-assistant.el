(use-package company
             :ensure t
             :defer t
             :config
             (global-company-mode)
             (setq company-idle-delay 0)
             (setq company-show-numbers t))

(use-package company-tabnine
  :requires (company)
  :ensure t)

(add-to-list 'company-backends #'company-tabnine)

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
		    '(("gemma" . (make-llm-ollama
				   :chat-model "gemma:2b"
				   :embedding-model "gemma:2b"))
		      ("mistral" . (make-llm-ollama
				    :chat-model "mistral:latest"
				    :embedding-model "mistral:latest"))
		      ("mixtral" . (make-llm-ollama
				    :chat-model "mixtral:8x7b"
				    :embedding-model "mixtral:8x7b"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
	    (make-llm-ollama
	     :chat-model "mistral:latest"
	     :embedding-model "mistral:latest"))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm))

(provide 'init-ai-assistant)
