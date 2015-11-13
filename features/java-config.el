;; java mode hook
;; dependency: dash s company

(add-hook 'java-mode-hook
          (lambda ()
            (custom-set-variables
             '(eclim-eclipse-dirs '("/opt/homebrew-cask/Caskroom/eclipse-jee/4.5/Eclipse.app/Contents/Eclipse"))
             '(eclim-executable "/opt/homebrew-cask/Caskroom/eclipse-jee/4.5/Eclipse.app/Contents/Eclipse/eclim")
             '(ac-auto-start nil)
             '(company-minimum-prefix-length 99))
            (add-to-list 'load-path "~/Desktop/Projects/emacs-eclim/")
            (require 'eclim)
            (global-eclim-mode)
            ;; add the emacs-eclim source
            ;; (require 'ac-emacs-eclim-source)
            ;; (ac-emacs-eclim-config)
            (require 'company)
            (require 'company-emacs-eclim)
            (company-emacs-eclim-setup)
            (company-mode t)
	    (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (load-file "~/.emacs.d/cscope/contrib/xcscope/xcscope.el")
            (setq cscope-do-not-update-database t)
            ;;(define-key java-mode-map (kbd "M-.") 'cscope-find-global-definition-no-prompting)
            (define-key java-mode-map (kbd "M-.") 'eclim-java-find-declaration)
            ;;(define-key java-mode-map (kbd "M-,") 'cscope-pop-mark)
            (define-key java-mode-map (kbd "M-,") 'pop-tag-mark)
            ;;(define-key java-mode-map (kbd "M-RET") 'jdee-complete)
            (define-key java-mode-map (kbd "M-RET") 'company-emacs-eclim)
            ))

(setq jdee-server-dir "/Users/ruan/.emacs.d/jdee-server/target")