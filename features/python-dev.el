
;;;;;;;;;;;;;;;;;;;;
;; Python IDE
;;;;;;;;;;;;;;;;;;;;
(use-package elpy
  :defer t
  :init (with-eval-after-load 'python (elpy-enable)))

(add-hook 'elpy-mode-hook
          (lambda ()
            (highlight-indentation-mode -1)
            (define-key elpy-mode-map (kbd "M-.")     'elpy-goto-definition)
            (define-key elpy-mode-map (kbd "C-x M-.") 'elpy-goto-definition-other-window)
            (define-key elpy-mode-map (kbd "M-,")     'pop-tag-mark)
            (define-key elpy-mode-map (kbd "M-RET")   'elpy-company-backend)))