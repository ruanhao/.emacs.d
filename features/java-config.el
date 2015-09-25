;; java mode hook
(add-hook 'java-mode-hook
          (lambda ()
	    (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (load-file "~/.emacs.d/cscope/contrib/xcscope/xcscope.el")
            (setq cscope-do-not-update-database t)
            (define-key java-mode-map (kbd "M-.") 'cscope-find-global-definition-no-prompting)
            (define-key java-mode-map (kbd "M-,") 'cscope-pop-mark)
            (define-key java-mode-map (kbd "M-RET") 'jdee-complete)))

(setq jdee-server-dir "/Users/ruan/.emacs.d/jdee-server/target")