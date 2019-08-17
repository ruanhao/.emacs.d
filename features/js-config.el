;;;;;;;;;;;;;;;;;;;;;;;;
;; js config
;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))

;; npm install -g typescript
;; jsconfig.json:
;; {
;;   "compilerOptions": {
;;     "target": "es2017",
;;     "allowSyntheticDefaultImports": true,
;;     "noEmit": true,
;;     "checkJs": true,
;;     "jsx": "react",
;;     "lib": [ "dom", "es2017" ]
;; }
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (unless (tide-current-server)
    (tide-restart-server))
  ;; (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(add-hook 'js2-mode-hook (lambda ()
                           (setup-tide-mode)
                           (define-key js2-mode-map (kbd "M-/") 'company-complete)
                           ))