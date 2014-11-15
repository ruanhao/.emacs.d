;; Ruby Config
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(setq rsense-home (expand-file-name "~/.emacs.d/rsense"))
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ruby-indent-level 4)
            (define-key ruby-mode-map (kbd "M-RET") 'ac-complete-rsense)
            (define-key ruby-mode-map (kbd "C-c t") 'rsense-type-help)
            (define-key ruby-mode-map (kbd "C-c w") 'rsense-where-is)
            (define-key ruby-mode-map (kbd "M-.")   'rsense-jump-to-definition)))

(require 'ruby-end)
