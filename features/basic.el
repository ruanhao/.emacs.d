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

;; Powerline is fancy
(require 'powerline)
(powerline-default-theme)

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
(global-set-key "\C-x\C-\\" 'goto-last-change)

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
  (load-theme 'moe-dark t)))

;; Setup moe-theme
(load-file "~/.emacs.d/themes/moe-theme.el")
(powerline-moe-theme)
;;(moe-theme-set-color 'orange)
(moe-theme-random-color)

(global-set-key (kbd "M--") 'kill-whole-line)

;; Set CUA-Utils
;; [M-a]: Leftify
;; [M-b]: Fill rect with space/tab
;; [M-c]: Remove all space on the left
;; [M-f]: Replace all characters in the rect by a specified char
;; [M-i]: Increment first number on every line
;; [M-k]: Cut rect
;; [M-l]: Downcase
;; [M-m]: Copy rect
;; [M-n]: Number the lines
;; [M-o]: Fill the rect with space in order to move right
;; [M-r]: Replace by regex
;; [M-R]: Revert up and down
;; [M-s]: Replace each line with a string
;; [M-t]: Replace the whole rect with a string
;; [M-u]: Uppercase
;; [M-|]: Do shell command on the content of the rect
(setq cua-enable-cua-keys nil)
(cua-mode)
(global-set-key (kbd "C-c SPC") 'cua-set-rectangle-mark)

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

;; (require 'sticky-windows)
;; (global-set-key [(control x) (?0)] 'sticky-window-delete-window)
;; (global-set-key [(control x) (?1)] 'sticky-window-delete-other-windows)
;; (global-set-key [(control x) (?9)] 'sticky-window-keep-window-visible)

;; Toggle window dedication

(defun hao-toggle-window-dedicated ()
"Toggle whether the current active window is dedicated or not"
(interactive)
(message
 (if (let (window (get-buffer-window (current-buffer)))
       (set-window-dedicated-p window
        (not (window-dedicated-p window))))
     "Window '%s' is dedicated"
   "Window '%s' is normal")
 (current-buffer)))
(global-set-key [(control x) (?9)] 'hao-toggle-window-dedicated)

;; ORG
(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-emphasis-alist
                         '("*" (:foreground "yellow" :background "red")))
            (org-indent-mode t)
            (setq truncate-lines nil)
            (setq org-src-fontify-natively t)
            (set-face-attribute 'org-level-1 nil :height 2.0 :bold t)
            (set-face-attribute 'org-level-2 nil :height 1.8 :bold t)
            (set-face-attribute 'org-level-3 nil :height 1.6 :bold t)))

;; Vertical split as default
(setq split-width-threshold nil)

;; Horizontal split as default
;; (setq split-width-threshold 1)

;; Ido
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

;; Case insensitive
(setq case-fold-search t)

(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)

;; nxml-mode-hook
(defun hao-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(require 'hideshow)
(require 'sgml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)
(add-hook 'nxml-mode-hook
	  (lambda ()
            (define-key nxml-mode-map (kbd "M-RET") 'completion-at-point)
            (define-key nxml-mode-map (kbd "C-c C-c") 'hs-toggle-hiding)
            (define-key nxml-mode-map (kbd "C-M-\\") 'hao-pretty-print-xml-region)))

(customize-set-variable 'nxml-slash-auto-complete-flag t)

;; Project Root
(load-file "~/.emacs.d/lisp/project-root.el")

(setq project-roots
      `(("Git Project"
         :root-contains-files (".gitignore"))))

(defun hao-ido-project-files ()
  "Use ido to select a file from the project."
  (interactive)
  (let (my-project-root project-files tbl)
    (unless project-details (project-root-fetch))
    (setq my-project-root (cdr project-details))
    ;; get project files
    (setq project-files
          (split-string
           (shell-command-to-string
            (concat "find "
                    my-project-root
                    " \\( -name \"*.svn\" -o -name \"*.git\" \\) -prune -o -type f -print | grep -E \"\.(java|xml|yang|el|org|xsd)$\" | grep -E -v \"(META-INF|WEB-INF|target|checkstyle|yang-gen)\"" ;; ADD FILENAME FILTER HERE
                    ;; example:
                    ;; " \\( -name \"*.svn\" -o -name \"*.git\" \\) -prune -o -type f -print | grep -E \"\.(java|xml|yang|el|org|xsd)$\" | grep -E -v \"(META-INF|WEB-INF|target|checkstyle)\""
                    )) "\n"))
    ;; populate hash table (display repr => path)
    (setq tbl (make-hash-table :test 'equal))
    (let (ido-list)
      (mapc (lambda (path)
              ;; format path for display in ido list
              (setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
              ;; strip project root
              (setq key (replace-regexp-in-string my-project-root "" key))
              ;; remove trailing | or /
              (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
              (puthash key path tbl)
              (push key ido-list)
              )
            project-files
            )
      (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl)))))
(global-set-key (kbd "C-x f") 'hao-ido-project-files)

;; Auto Revert
(global-auto-revert-mode t)

;; yang-mode
(load-file "~/.emacs.d/lisp/yang-mode.el")

;; my-keys-minor-mode-map
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "M-j") 'move-line-down)
(define-key my-keys-minor-mode-map (kbd "M-k") 'move-line-up)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t "my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

;; yang-mode-hook
(add-hook 'yang-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq c-basic-offset 4)))

;; ag
(require 'ag)
(setq ag-highlight-search t)