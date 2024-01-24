
;;;;;;;;;;;;;;;;;;;;
;; Copilot Configuration
;; M-x package-install RET editorconfig RET
;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/copilot.el")
(require 'copilot)
;; (add-to-list 'copilot-major-mode-alist '("python"))
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)