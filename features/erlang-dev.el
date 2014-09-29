;; erlang develop
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
  "Find pair for if, case, begin for Erlang mode"
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

;; Emacs Erlang mode setup
(setq load-path (cons "/lab/testtools/rhel664/otp/R15B01_halfword/lib/erlang/lib/tools-2.6.7/emacs"
		      load-path))
(setq erlang-root-dir "/lab/testtools/rhel664/otp/R15B01_halfword/lib/erlang/erts-5.9.1")
(setq exec-path (cons "/lab/testtools/rhel664/otp/R15B01_halfword/lib/erlang/erts-5.9.1/bin" exec-path))

(require 'erlang-start)

(add-to-list 'auto-mode-alist '("\\.\\(erl\\|hrl\\|app\\|app.src\\)" . erlang-mode))

;; erlang-mode-hook
(add-hook 'erlang-mode-hook
          (lambda ()
	    (setq indent-tabs-mode nil)
            (erlang-font-lock-level-3)
	    (modify-syntax-entry ?_ "w")
            ;; when starting an Erlang shell in Emacs, set default node name
            (setq inferior-erlang-machine-options '("-sname" "emacs" "-setcookie" "emacs"))))

(global-set-key (kbd "M-.") (lambda () (interactive) (find-tag (find-tag-default))))
(global-set-key (kbd "M-,") 'pop-tag-mark)
