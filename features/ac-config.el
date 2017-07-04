
;; auto-complete
;; (add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)
(ac-linum-workaround) ;; fix auto-complete-mode and linum-mode annoyance
(define-key ac-mode-map (kbd "M-/") 'auto-complete)
