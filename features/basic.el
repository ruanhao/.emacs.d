;; -*- coding: utf-8 -*-
;; author: Hao Ruan
;; date: 2013/09/27

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Can't live without C-h
(define-key key-translation-map [?\C-h] [?\C-?])

;; I like tap Ctrl-J to indent, not Enter
(electric-indent-mode 0)

(setq default-major-mode 'text-mode)

(add-hook 'before-save-hook (lambda ()
                              (unless (equal "md" (file-name-extension (buffer-file-name)))
                                (delete-trailing-whitespace))))

(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-default nil)

;; No automatically newline
(setq mode-require-final-newline nil)

;; Can use Ctrl-<right> Ctrl-<left> to toggle sessions
(when (fboundp 'winner-mode)
  (winner-mode 1))
;; (global-superword-mode t)
(line-number-mode t)
(column-number-mode t)
;; (global-hl-line-mode 1)
(set-face-foreground 'highlight nil)
;; (set-face-background 'hl-line "#262626")
(set-face-attribute 'region nil :background "#808080")

;; Dynamicaly update line number
(require 'linum)
(global-linum-mode t)
(defadvice linum-update-window (around linum-format-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; Go to last change
(require 'goto-last-change)
(global-set-key (kbd "C-c C-c") 'goto-last-change)

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
(global-set-key [f7] 'whitespace-mode)

;; emacs-lisp-mode-hook
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?- "w")
	    (setq indent-tabs-mode nil)))

;; Parenthesis pair utilities
(show-paren-mode t)
(load-file "~/.emacs.d/lisp/autopair.el")
(autopair-global-mode)

(setq tags-revert-without-query t)

;; Set molokai theme
(cond
 ((= emacs-major-version 23)
  (add-to-list 'load-path "~/.emacs.d/themes/")
  (require 'molokai-theme))
 ((>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'molokai t)))

(global-set-key (kbd "M--") 'kill-whole-line)

;; Set CUA-Utils
;; (setq cua-enable-cua-keys nil)
;; (cua-mode)
;; (global-set-key [f5] 'cua-set-rectangle-mark)

;; Set multiple-cursors
(add-to-list 'load-path "~/.emacs.d/multiple-cursors")
(require 'multiple-cursors)
(global-set-key [f8] 'mc/mark-more-like-this-extended)

;; Set delete-pair
(global-set-key (kbd "M-)") 'delete-pair)

;; Delete pair backward
(defun hao-delete-pair-backward ()
  "Delete a pair of characters enclosing the sexp that follows point (backward)."
  (interactive)
  (save-excursion (backward-sexp 1) (delete-char 1))
  (delete-char -1))
(global-set-key (kbd "M-(") 'hao-delete-pair-backward)

;; Set direction
;; (global-set-key (kbd "<down>") 'next-logical-line)
;; (global-set-key (kbd "<up>") 'previous-logical-line)

;; Dirtree
(add-to-list 'load-path "~/.emacs.d/dirtree")
(global-set-key [f2] 'dirtree)
(require 'dirtree)

;; (require 'neotree)
;; (global-set-key [f2] 'neotree-toggle)

;; Bookmark
;; (define-key global-map [f9] 'bookmark-set)
;; (define-key global-map [f10] 'bookmark-jump)
;; (setq bookmark-save-flag 1)

;; Revert buffer
;; (define-key global-map [f11] 'revert-buffer)

;; Markerpen
(load-file "~/.emacs.d/lisp/markerpen.el")

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(add-to-list 'load-path "~/.emacs.d/expand-region")
(require 'expand-region)
(global-set-key (kbd "M-@") 'er/expand-region)

(global-set-key [(f9)] 'shrink-window)
(global-set-key [(f10)] (lambda () (interactive) (shrink-window -1)))

(global-set-key [(f11)] 'shrink-window-horizontally)
(global-set-key [(f12)] (lambda () (interactive)(shrink-window-horizontally -1)))

(global-set-key [(f5)] (lambda () (interactive)(window-configuration-to-register 'w)))
(global-set-key [(f6)] (lambda () (interactive)(jump-to-register 'w)))

(require 'sticky-windows)
(global-set-key [(control x) (?0)] 'sticky-window-delete-window)
(global-set-key [(control x) (?1)] 'sticky-window-delete-other-windows)
(global-set-key [(control x) (?9)] 'sticky-window-keep-window-visible)
