;;;;;;;;;;;;;;;;;;;;
;; load-path
;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/lisp")

;;;;;;;;;;;;;;;;;;;;;;;;
;; personal information
;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Hao Ruan"
      user-mail-address "ruanhao1116@google.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package setup
;; use M-x package-refresh-contents to reload the list of packages for the first time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq  package-archives
       '(("gnu" . "http://elpa.gnu.org/packages/")
         ("org" . "http://orgmode.org/elpa/")
         ("melpa" . "http://melpa.org/packages/")
         ("melpa-stable" . "http://stable.melpa.org/packages/"))
       package-archive-priorities '(("melpa-stable" . 1)))
(package-initialize)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables
;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq-default
 indent-tabs-mode nil
 tab-width        4
 c-basic-offset   4
 )

(setq
 custom-file "~/.emacs.d/custom-settings.el"
 default-major-mode               'text-mode
 mac-option-modifier              'ctrl
 mac-command-modifier             'meta
 show-paren-delay                 0
 bookmark-save-flag               1
 scroll-step                      1
 scroll-conservatively            9999
 split-width-threshold            nil
 create-lockfiles                 nil
 make-backup-files                nil
 auto-save-default                nil
 mode-require-final-newline       nil
 case-fold-search                 t
 column-number-mode               t
 inhibit-splash-screen            t
 tags-revert-without-query        t
 set-mark-command-repeat-pop      t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; global-set-key
;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [(f9)] 'shrink-window)
(global-set-key [(f10)] (lambda () (interactive) (shrink-window -1)))

(global-set-key [(f11)] 'shrink-window-horizontally)
(global-set-key [(f12)] (lambda () (interactive)(shrink-window-horizontally -1)))

(global-set-key [(f5)] (lambda () (interactive)(window-configuration-to-register 'w)))
(global-set-key [(f6)] (lambda () (interactive)(jump-to-register 'w)))

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map (kbd "M-j") 'move-line-down)
(define-key my-keys-minor-mode-map (kbd "M-k") 'move-line-up)
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t "my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook
	      (lambda ()
	        (modify-syntax-entry ?- "w")
	        (setq indent-tabs-mode nil)))

(add-hook 'before-save-hook
          (lambda ()
            (unless (equal "md" (file-name-extension (buffer-file-name)))
              (delete-trailing-whitespace))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-emphasis-alist
                         '("*" (:foreground "yellow" :background "red")))
            (org-indent-mode t)
            (setq truncate-lines nil)
            (setq org-src-fontify-natively t)
            (modify-syntax-entry ?= "w")
            (set-face-attribute 'org-level-1 nil :height 2.0 :bold t)
            (set-face-attribute 'org-level-2 nil :height 1.8 :bold t)
            (set-face-attribute 'org-level-3 nil :height 1.6 :bold t)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; personal preferences
;;;;;;;;;;;;;;;;;;;;;;;;
(define-key key-translation-map [?\C-h] [?\C-?])
(fset 'yes-or-no-p 'y-or-n-p)
(save-place-mode 1)
(menu-bar-mode -1)
(electric-indent-mode 0)
(prefer-coding-system 'utf-8)
(show-paren-mode t)
(global-auto-revert-mode t)
(global-hi-lock-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; key-chord
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package key-chord
  :init (require 'key-chord)
  :config (progn
            (key-chord-mode 1)
            (setq key-chord-one-key-delay  1.0)
            (setq key-chord-two-keys-delay 1.0)))
;; (key-chord-define-global "rp" 'point-to-register)
;; (key-chord-define-global "rj" 'register-to-point)

;;;;;;;;;;;;;;;;;;;;;;;;
;; expand-region
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :defer t
  :bind ("M-@" . er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight
;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package highlight-symbol
;;   :defer t
;;   :init (require 'highlight-symbol)
;;   :config (progn
;;             (global-set-key [f4] 'highlight-symbol)))

(use-package hl-anything
  :pin melpa-stable
  :defer t
  :init (hl-highlight-mode t)
  :config (global-set-key [f3] (lambda ()
                                 (interactive)
                                 (hl-highlight-thingatpt-local)
                                 (deactivate-mark))))

;;;;;;;;;;;;;;;;;;;;
;; ace-jump-mode
;;;;;;;;;;;;;;;;;;;;
(use-package ace-jump-mode
  :defer t)
(key-chord-define-global "jj" 'ace-jump-char-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :diminish yas-minor-mode
  :defer t
  :init (yas-global-mode)
  :config (progn
            (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
            (setq yas-key-syntaxes '("w_" "w_." "^ "))
            (setq yas-snippet-dirs '("~/.emacs.d/yasnippet/snippets"))
            (setq yas-expand-only-for-last-commands nil)
            (setq yas-indent-line nil)
            (yas-global-mode 1)
            (bind-key "\t" 'hippie-expand yas-minor-mode-map)))

;;;;;;;;;;;;;;;;;;;;
;; ag
;;;;;;;;;;;;;;;;;;;;
(use-package ag
  :pin melpa                            ; vertion should be [0.48,)
  :config (progn
            (global-set-key (kbd "M-s g") 'ag-project-files)
            ;; (setq grep-highlight-matches t)
            ;; (setq grep-command "grep --color=auto -iInRH * --regexp=")
            (setq ag-highlight-search t)
            (setq ag-arguments (list "--smart-case" "--column")) ;; fix bug on MacOS
            ))

;;;;;;;;;;;;;;;;;;;;
;; auto-complete
;;;;;;;;;;;;;;;;;;;;
(use-package auto-complete
  :config (progn
            (ac-config-default)
            (setq ac-auto-start 4)
            (global-auto-complete-mode t)
            (ac-linum-workaround) ;; fix auto-complete-mode and linum-mode annoyance
            (define-key ac-mode-map (kbd "M-/") 'auto-complete)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package powerline
  :init (powerline-default-theme))


;;;;;;;;;;;;;;;;;;;;;;;;
;; nlinum
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nlinum
  :init (global-nlinum-mode t))
(defun my-nlinum-mode-hook ()
  (when nlinum-mode
    (setq-local nlinum-format
                (concat "%" (number-to-string
                             ;; Guesstimate number of buffer lines.
                             (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                        "d "))))
(add-hook 'nlinum-mode-hook 'my-nlinum-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;
;; whitespace-mode
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package whitespace
  :bind ([f7] . whitespace-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package autopair
  :init (autopair-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; theme
;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package molokai-theme
;;   :demand)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'solarized-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; CUA
;;;;;;;;;;;;;;;;;;;;;;;;
(setq cua-enable-cua-keys nil)
(cua-mode)
(global-set-key (kbd "C-c SPC") 'cua-set-rectangle-mark)

;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :init (require 'multiple-cursors)
  :bind ([f8] . mc/mark-more-like-this-extended))

;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :init (require 'undo-tree)
  :config (progn
            (global-undo-tree-mode t)
            (global-set-key (kbd "M--") 'undo-tree-redo)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; fill column indicator
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fill-column-indicator
  :defer t)
(defun hao-toggle-column-ruler ()
  "Toggle column ruler"
  (interactive)
  (setq fill-column 80)
  (unless (boundp 'hao-toggle-column-ruler)
    (setq hao-toggle-column-ruler nil))
  (if (not hao-toggle-column-ruler)
      (fci-mode 1)
    (fci-mode 0))
  (setq hao-toggle-column-ruler (not hao-toggle-column-ruler)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
;;;;;;;;;;;;;;;;;;;;;;;;
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package recentf
  :defer t
  :init (require 'recentf)
  :config (progn
            (recentf-mode 1)
            (setq recentf-max-menu-item 10))
  :bind ("C-x C-r" . 'recentf-open-files)
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package neotree
  :bind ([f2] . neotree-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;
;; project root
;;;;;;;;;;;;;;;;;;;;;;;;
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
                    " \\( -name \"*.svn\" -o -name \"*.git\" \\) -prune -o -type f -print | grep -E \"\.(py|java|xml|yang|el|org|xsd|jsp)$\" | grep -E -v \"(META-INF|WEB-INF|target|checkstyle|yang-gen)\"" ;; ADD FILENAME FILTER HERE
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



;;;;;;;;;;;;;;;;;;;;;;;;
;; org
;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-log-done t)
(setq org-html-postamble t)
(setq org-html-postamble-format
      '(("en"
         "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Created: %d</p>
<p class=\"date\">Updated: %C</p>
")))

;; %t stands for the title.
;; %a stands for the author's name.
;; %e stands for the author's email.
;; %d stands for the date.
;; %c will be replaced by `org-html-creator-string'.
;; %v will be replaced by `org-html-validation-link'.
;; %T will be replaced by the export time.
;; %C will be replaced by the last modification time.




;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :defer t
  :config (global-company-mode 1)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable annoying error
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)
