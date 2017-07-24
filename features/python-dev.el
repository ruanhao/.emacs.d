
;;;;;;;;;;;;;;;;;;;;
;; Python IDE
;;;;;;;;;;;;;;;;;;;;
(use-package elpy
  :demand)
(elpy-enable)
(add-hook 'elpy-mode-hook
          (lambda ()
            (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition)
            (define-key elpy-mode-map (kbd "C-x M-.") 'elpy-goto-definition-other-window)
            (define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)
            (define-key elpy-mode-map (kbd "M-RET") 'elpy-company-backend)
            (highlight-indentation-mode -1)
            ))