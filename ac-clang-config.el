(require 'auto-complete-clang)
;; (setq ac-auto-start t)
(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
(define-key ac-mode-map (kbd "\C-m") 'ac-complete-clang)
;; To find the include files in you system, please do:
;; $ echo "" | g++ -v -x c++ -E -
(defun my-ac-config ()
  (setq ac-clang-flags
        (mapcar(lambda (item)(concat "-I" item))
               (split-string
                "
 /usr/include/c++/4.4.7
 /usr/include/c++/4.4.7/x86_64-redhat-linux
 /usr/include/c++/4.4.7/backward
 /usr/local/include
 /usr/lib/gcc/x86_64-redhat-linux/4.4.7/include
 /usr/include
    ")))
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)
