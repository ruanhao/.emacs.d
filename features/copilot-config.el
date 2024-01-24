;;;;;;;;;;;;;;;;;;;;
;; Copilot Configuration
;;;;;;;;;;;;;;;;;;;;

(use-package s
  :straight (:host github :repo "magnars/s.el");; :files ("s.el"))
  :ensure t)

(use-package dash
  :straight (:host github :repo "magnars/dash.el");; :files ("dash.el"))
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

(require 'copilot)
;; (add-to-list 'copilot-major-mode-alist '("python"))
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)