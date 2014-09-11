;; -*- coding: utf-8 -*-
;; author: Hao Ruan
;; date: 2013/09/27

(add-to-list 'load-path "~/.emacs.d/")

;; Can't live without C-h
(define-key key-translation-map [?\C-h] [?\C-?])

(setq default-major-mode 'text-mode)

(add-hook 'before-save-hook (lambda ()
                              (unless (equal "md" (file-name-extension (buffer-file-name)))
                                (delete-trailing-whitespace))))

(setq backup-inhibited t)
(setq auto-save-default nil)

(line-number-mode t)
(column-number-mode t)

;; Dynamicaly update line number
(require 'linum)
(global-linum-mode t)
(defadvice linum-update-window (around linum-format-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; yes or no
(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)

;; Display time and system load
(display-time)

(setq save-place t)
(require 'saveplace)

;; Scroll line by line
(setq scroll-step 1)
(setq scroll-conservatively 9999)

;; Show whitespace
(require 'whitespace)
(global-set-key [f6] 'whitespace-mode)

;; emacs-lisp-mode-hook
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?- "w")
	    (setq indent-tabs-mode nil)))

;; Parenthesis pair utilities
(show-paren-mode t)
(load-file "~/.emacs.d/autopair.el")
(autopair-global-mode)

(setq tags-revert-without-query t)

;; Set molokai theme
(cond
 ((= emacs-major-version 23)
  (add-to-list 'load-path "~/.emacs.d/themes/")
  (require 'molokai-theme))
 ((= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'molokai t)))

(global-set-key (kbd "M--") 'kill-whole-line)

;; Set CUA-Utils
(setq cua-enable-cua-keys nil)
(cua-mode)
(global-set-key [f5] 'cua-set-rectangle-mark)

;; Set multiple-cursors
(add-to-list 'load-path "~/.emacs.d/multiple-cursors")
(require 'multiple-cursors)
(global-set-key [f8] 'mc/mark-next-word-like-this)
(global-set-key [f7] 'mc/mark-previous-word-like-this)

;; Set delete-pair
(global-set-key (kbd "M-)") 'delete-pair)
