(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (c-set-style "linux")
             (flyspell-prog-mode)
	     (setq indent-tabs-mode nil)
	     (setq c-basic-offset 4)
             (modify-syntax-entry ?_ "w")

             ;; cscope setup
             (load-file "~/.emacs.d/cscope/contrib/xcscope/xcscope.el")
             (setq cscope-do-not-update-database t)
             (define-key c-mode-map (kbd "M-.") 'cscope-find-global-definition-no-prompting)
             (define-key c-mode-map (kbd "M-,") 'cscope-pop-mark)

             (define-key c-mode-map (kbd "M-RET") 'ac-complete-clang)))

(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))

(add-hook 'c++-mode-hook 'my:ac-c-headers-init)
(add-hook 'c-mode-hook 'my:ac-c-headers-init)