
;;;;;;;;;;;;;;;;;;;;
;; Python IDE
;; pip instal jedi virtualenv

;; M-x package-install RET jedi
;; M-x jedi:install-server
;; If run into [deferred error : (error "\"AttributeError(\\\"type object 'Script' has no attribute 'goto_assignments'\\\")\"")'], try:
;;  ~/.emacs.d/.python-environments/default/bin/pip install jedi==0.17.2

;; Reference:
;; https://tkf.github.io/emacs-jedi/latest/
;; https://github.com/tkf/emacs-jedi/issues/352
;;;;;;;;;;;;;;;;;;;;
(use-package elpy
  :defer t
  :init (with-eval-after-load 'python (elpy-enable))
  :config (progn
            (setq hao-python-split-direction 0) ;; used to split window
            )
  )

(defun hao-find-python-definition-other-window ()
  (interactive)
  ;; (ag-project-files (ag/dwim-at-point) (list :file-regex (ag/buffer-extension-regex)))
  (if (cl-oddp hao-python-split-direction)
      (hao-open-window-vertically-friendly)
    (hao-open-window-horizontally-friendly)
      )
  (setq hao-python-split-direction (+ hao-python-split-direction 1))
  (jedi:goto-definition)
  )

(add-hook 'elpy-mode-hook
          (lambda ()
            (highlight-indentation-mode -1)
                        (setq jedi:setup-keys t)
            (setq jedi:complete-on-dot t)
            (setq elpy-rpc-python-command "python3")
            (setq elpy-rpc-backend "jedi")
            ;; (define-key elpy-mode-map (kbd "M-.")     'elpy-goto-definition)
            ;; (define-key elpy-mode-map (kbd "M-,")     'pop-tag-mark)
            ;; (define-key elpy-mode-map (kbd "C-x M-.") 'elpy-goto-definition-other-window)
            ;; (define-key elpy-mode-map (kbd "M-RET")   'elpy-company-backend)

            (define-key elpy-mode-map (kbd "M-.")     'jedi:goto-definition)
            (define-key elpy-mode-map (kbd "M-,")     'jedi:goto-definition-pop-marker)
            (define-key elpy-mode-map (kbd "C-x M-.") 'hao-find-python-definition-other-window)
            (define-key elpy-mode-map (kbd "M-RET")   'jedi:complete)
            ))
(add-hook 'python-mode-hook 'jedi:setup)