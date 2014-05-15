;; -*- coding: utf-8 -*-
;; author: Hao Ruan
;; date: 2013/09/27

;; hao-emacs-customization

(defun hao-show-system-type ()
  "find out what OS Emacs is currently running on"
  (interactive)
  (message "%s" system-type))

(defun hao-show-features-loaded ()
  "find out all features loaded"
  (interactive)
  (message "%S" features))

(defun hao-pick-word-at-point ()
  "pick current word under cursor
this function would move cursor to the beginning of the word"
  (let (tail-point)
    (skip-chars-forward "-_A-Za-z0-9")
    (setq tail-point (point))
    (skip-chars-backward "-_A-Za-z0-9")
    (buffer-substring-no-properties (point) tail-point)))

(defun hao-pick-regexp-word-at-point ()
  "pick current regexp word at point"
  (save-excursion
      (concat "\\b" (hao-pick-word-at-point) "\\b")))

;; highlight word at point
;; bind to [f3]
(defun hao-highlight-word-at-point ()
  "highlight the word at point"
  (interactive)
  (let ((regexp-word (hao-pick-regexp-word-at-point)) color hi-colors)
    (unless (boundp 'hao-highlight-word-at-point)
      (setq hao-highlight-word-at-point 0))
    (unhighlight-regexp regexp-word)
    (add-to-list 'regexp-search-ring regexp-word)
    ;; only 4 highlight colors supported now
    (setq hi-colors '("hi-yellow" "hi-pink" "hi-green" "hi-blue"))
    (setq color
	  (nth (% hao-highlight-word-at-point (length hi-colors)) hi-colors))
    (highlight-regexp regexp-word color)
    (setq hao-highlight-word-at-point (1+ hao-highlight-word-at-point))))
(global-set-key [f3] 'hao-highlight-word-at-point)

(defun hao-unhighlight-all ()
  "unhighlight all highlighted words"
  (interactive)
  ;; in case of a lot of overlays
  (dotimes (i 10)
    (mapc (lambda (regex) (unhighlight-regexp regex))
	(append regexp-history regexp-search-ring))))

(defun hao-unhighlight-word-at-point ()
  "unhighlight the word at point"
  ;; in case of a lot of overlays
  (interactive)
  (dotimes (i 10)
    (unhighlight-regexp (hao-pick-regexp-word-at-point))))

(defun hao-buffer-menu-friendly ()
  "show buffer menu friendly"
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (buffer-menu))
(global-set-key (kbd "\C-x \C-b") 'hao-buffer-menu-friendly)

(defun hao-open-window-horizontally-friendly ()
  "open a new window at right side and move into it"
  (interactive)
  (split-window-horizontally)
  (windmove-right))
  (global-set-key (kbd "\C-x 3") 'hao-open-window-horizontally-friendly)

(defun hao-open-window-vertically-friendly ()
  "open a new widow at beneth side and move into it"
  (interactive)
  (split-window-vertically)
  (windmove-down))
(global-set-key (kbd "\C-x 2") 'hao-open-window-vertically-friendly)

(defun hao-other-window-backward ()
  "similar to other-window but backward"
  (interactive)
  (other-window -1))
(global-set-key (kbd "M-n") 'other-window)
(global-set-key (kbd "M-p") 'hao-other-window-backward)

(defun hao-kill-till-beginning-of-line()
  "delete reversely till head of current line"
  (interactive)
  (kill-line 0))
(global-set-key (kbd "\C-u") 'hao-kill-till-beginning-of-line)

;; (defun hao-kill-till-end-of-line()
;;   "delete till end of line"
;;   (interactive)
;;   (let ((current-point (point)))
;;     (kill-line)
;;     (skip-chars-forward "\s\t")
;;     (delete-region current-point (point))))
;; (global-set-key (kbd "\C-k") 'hao-kill-till-end-of-line)

(defun hao-join-lines ()
  "merge lines into one line"
  (interactive)
  (let* ((rbegin (region-beginning))
         (rend (region-end))
         (num-of-lines (count-lines rbegin rend))
         (bottom-position (max rbegin rend)))
    (goto-char bottom-position)
    (dotimes (i (1- num-of-lines))
      (join-line))))

(defun hao-same-line ()
  "set all windows at the same line"
  (interactive)
  (let ((line-number (line-number-at-pos)))
    (dotimes (i (length (window-list)))
      (goto-line line-number)
      (recenter-top-bottom)
    (other-window 1))))
(global-set-key (kbd "\C-c l") 'hao-same-line)

(defun hao-erlang-pair-keyword-valid-p ()
  (let ((line-head-point (line-beginning-position)))
    (if (and
         (= 0 (% (count-matches "\"" line-head-point (point)) 2))
         (= 0 (% (count-matches "'" line-head-point (point)) 2))
         (= 0 (count-matches "%" line-head-point (point))))
        t
      nil)))

(defun hao-erlang-pair-construct-stack (value old-stack)
  (if (= 0 (+ value (car old-stack)))
      (cdr old-stack)
    (cons value old-stack)))

(defun hao-erlang-pair-find (direction stack origin-point)
  (catch 'ok
    (while t
      (condition-case nil
          (progn
            (funcall direction "\\(^\\|[\s\t\\[(=>]\\)\\(case\\|if\\|begin\\|receive\\|fun[\s\t\n]*(\.*\\|end\\)\\($\\|[\s\t,;.]\\)")
            (goto-char (match-beginning 2))
            (setq stack
                  (if (not (hao-erlang-pair-keyword-valid-p))
                      stack
                    (if (looking-at "end")
                        (hao-erlang-pair-construct-stack -1 stack)
                      (hao-erlang-pair-construct-stack 1 stack))))
            (if stack
                (forward-char)          ; a trick here, there is no need to use
                                        ; (backward-char) here when do backward-search,
                                        ; but you have to use (forward-char) when do forward-search
              (throw 'ok t)))
        (error (progn
                 (message "Wrong format")
                 (goto-char origin-point)
                 (throw 'ok t)))))))

(defun hao-erlang-pair ()
  "find pair for if, case, begin for Erlang mode"
  (interactive)
  (when (eq major-mode 'erlang-mode)
    (let ((keywords '("case" "if" "begin" "receive")))
      (when (hao-erlang-pair-keyword-valid-p)
        (if (or (member (hao-pick-word-at-point) keywords)
                (looking-at "fun[\s\t\n]*(")) ; 'fun' is an except
            (progn
              (forward-char)
              (hao-erlang-pair-find 'search-forward-regexp '(1) (point)))
          (if (equal (hao-pick-word-at-point) "end")
              (progn
                ;; (backward-char)
                (hao-erlang-pair-find 'search-backward-regexp '(-1) (point)))))))))

;; toggle window form horizontal to vertical and vice versa
(defun hao-toggle-window ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; Emacs customization basic part

;; set default mode
(setq default-major-mode 'text-mode)

;; (if (not (equal major-mode 'text-mode))
;;     (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; can't live without C-h
(define-key key-translation-map [?\C-h] [?\C-?])

;; prevent Emacs from making backup files
(setq backup-inhibited t)
(setq auto-save-default nil)

;; enable line and column numbering
(line-number-mode t)
(column-number-mode t)

;; add line number
(require 'linum)
(global-linum-mode t)
(defadvice linum-update-window (around linum-format-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; no menu-bar, welcome screen at startup and tool-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)

(setq tags-revert-without-query t)

;; display time and system load
(display-time)

;; save place
(setq save-place t)
(require 'saveplace)

;; scroll line by line
(setq scroll-step 1)
(setq scroll-conservatively 9999)

;; delete whole line
(global-set-key (kbd "M-9") 'kill-whole-line)

;; show whitespace
(require 'whitespace)
(global-set-key [f6] 'whitespace-mode)

;; Emacs customization advanced part

;; fill column indicator
(load-file "~/.emacs.d/fill-column-indicator.el")
(defun hao-toggle-column-ruler ()
  "toggle column ruler"
  (interactive)
  (setq fill-column 80)
  (unless (boundp 'hao-toggle-column-ruler)
    (setq hao-toggle-column-ruler nil))
  (if (not hao-toggle-column-ruler)
      (fci-mode 1)
    (fci-mode 0))
  (setq hao-toggle-column-ruler (not hao-toggle-column-ruler)))
(global-set-key [f7] 'hao-toggle-column-ruler)


;; set molokai theme
(cond
 ((= emacs-major-version 23)
  (add-to-list 'load-path "~/.emacs.d/themes/")
  (require 'molokai-theme))
 ((= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'molokai t)))

;; Emacs Erlang mode setup
(setq load-path (cons "/lab/testtools/rhel664/otp/R15B01_halfword/lib/erlang/lib/tools-2.6.7/emacs"
		      load-path))
(setq erlang-root-dir "/lab/testtools/rhel664/otp/R15B01_halfword/lib/erlang/erts-5.9.1")
(setq exec-path (cons "/lab/testtools/rhel664/otp/R15B01_halfword/lib/erlang/erts-5.9.1/bin" exec-path))

(require 'erlang-start)
(add-to-list 'auto-mode-alist '("\\.\\(erl\\|hrl\\|app\\|app.src\\)" . erlang-mode))

;; distel setup
(add-to-list 'load-path "~/.emacs.d/distel/elisp")
(require 'distel)
(distel-setup)
(setq derl-cookie "emacs")

;; parenthesis pair utilities
(show-paren-mode t)
(load-file "~/.emacs.d/autopair.el")
(autopair-global-mode)

;; emacs-lisp-mode-hook
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?- "w")
	    (setq indent-tabs-mode nil)))

;; kernel style
;; (add-hook 'c-mode-common-hook
;; 	  '(lambda ()
;; 	     (c-set-style "linux")
;; 	     (setq indent-tabs-mode nil)
;; 	     (setq c-basic-offset 4)
;;              ;; cscope setup
;;              (load-file "~/.emacs.d/cscope/contrib/xcscope/xcscope.el")
;; ))

;; erlang-mode-hook
(add-hook 'erlang-mode-hook
          (lambda ()
	    (setq indent-tabs-mode nil)
            (erlang-font-lock-level-3)
	    (modify-syntax-entry ?_ "w")
            ;; when starting an Erlang shell in Emacs, set default node name
            (setq inferior-erlang-machine-options '("-sname" "emacs" "-setcookie" "emacs"))))

;; yasnippet
;; (load-file "~/.emacs.d/yasnippet/yasnippet.el")
;; (setq yas/snippet-dirs "~/.emacs.d/yasnippet/snippets")
;; (yas/global-mode 1)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)
