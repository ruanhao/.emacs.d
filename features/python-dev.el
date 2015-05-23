(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)
        ;; cscope setup
        (load-file "~/.emacs.d/cscope/contrib/xcscope/xcscope.el")
        (setq cscope-do-not-update-database t)
        (define-key python-mode-map (kbd "M-.") 'cscope-find-global-definition-no-prompting)
        (define-key python-mode-map (kbd "M-,") 'cscope-pop-mark)
        (define-key python-mode-map (kbd "M-RET") 'jedi:complete)
        (jedi:setup)
        ;;(setq jedi:complete-on-dot t)
        ))
