
;;;;;;;;;;;;;;;;;;;;
;; Java IDE
;;;;;;;;;;;;;;;;;;;;
(use-package s
  :demand)
(use-package dash
  :demand)
(use-package company
  :demand)
(add-hook 'java-mode-hook
          (lambda ()
            (custom-set-variables
             ;; '(eclim-eclipse-dirs '("/opt/homebrew-cask/Caskroom/eclipse-java/4.5.2/Eclipse.app/Contents/Eclipse"))
             '(eclim-eclipse-dirs '("/Users/haoruan/Applications/Eclipse.app/Contents/Eclipse"))
             ;; '(eclim-executable "/opt/homebrew-cask/Caskroom/eclipse-java/4.5.2/Eclipse.app/Contents/Eclipse/eclim")
             '(eclim-executable "/Users/haoruan/Applications/Eclipse.app/Contents/Eclipse/eclim")
             '(ac-auto-start nil)
             '(company-minimum-prefix-length 99))
            (add-to-list 'load-path "~/Desktop/Projects/emacs-eclim/")
            (require 'eclim)
            ;; (global-eclim-mode)
            (eclim-mode)
            ;; add the emacs-eclim source
            ;; (require 'ac-emacs-eclim-source)
            ;; (ac-emacs-eclim-config)
            (require 'company)
            (require 'company-emacs-eclim)
            (company-emacs-eclim-setup)
            (global-company-mode t)
	        (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (define-key java-mode-map (kbd "M-.") 'eclim-java-find-declaration)
            (define-key java-mode-map (kbd "C-x M-.") (lambda ()
                                                        (interactive)
                                                        (hao-open-window-vertically-friendly)
                                                        (eclim-java-find-declaration)
                                                        ))
            (define-key java-mode-map (kbd "M-,") 'pop-tag-mark)
            (define-key java-mode-map (kbd "M-RET") 'company-emacs-eclim)
            ))
(defun hilite-todos ()
  (highlight-lines-matching-regexp "\\<\\(FIXME\\|WRITEME\\|WRITEME!\\|TODO\\|BUG\\):?"
                                   'hi-red-b)
  )
(add-hook 'java-mode-hook 'hilite-todos)