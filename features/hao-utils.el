
;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities functions
;;;;;;;;;;;;;;;;;;;;;;;;
;; delete pair forward
(global-set-key (kbd "M-)") 'delete-pair)
;; delete pair backward
(defun hao-delete-pair-backward ()
  "Delete a pair of characters enclosing the sexp that follows point (backward)."
  (interactive)
  (save-excursion (backward-sexp 1) (delete-char 1))
  (delete-char -1))
(global-set-key (kbd "M-(") 'hao-delete-pair-backward)

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

(defun hao-pretty-print-xml-region (begin end)
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun hao-buffer-menu-friendly ()
  "Show buffer menu friendly"
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (buffer-menu))
(global-set-key (kbd "\C-x \C-b") 'hao-buffer-menu-friendly)

(defun hao-open-window-horizontally-friendly ()
  "Open a new window at right side and move into it"
  (interactive)
  (split-window-horizontally)
  (windmove-right))
(global-set-key (kbd "\C-x 3") 'hao-open-window-horizontally-friendly)

(defun hao-open-window-vertically-friendly ()
  "Open a new widow at beneth side and move into it"
  (interactive)
  (split-window-vertically)
  (windmove-down))
(global-set-key (kbd "\C-x 2") 'hao-open-window-vertically-friendly)

(defun hao-other-window-backward ()
  "Similar to other-window but backward"
  (interactive)
  (other-window -1))
(global-set-key (kbd "M-n") 'other-window)
(global-set-key (kbd "M-p") 'hao-other-window-backward)

(defun hao-kill-till-beginning-of-line()
  "Delete reversely till head of current line"
  (interactive)
  (kill-line 0))
(global-set-key (kbd "\C-u") 'hao-kill-till-beginning-of-line)

(defun hao-join-lines ()
  "Merge lines into one line"
  (interactive)
  (let* ((rbegin (region-beginning))
         (rend (region-end))
         (num-of-lines (count-lines rbegin rend))
         (bottom-position (max rbegin rend)))
    (goto-char bottom-position)
    (dotimes (i (1- num-of-lines))
      (join-line))))

(defun hao-same-line ()
  "Set all windows at the same line"
  (interactive)
  (let ((line-number (line-number-at-pos)))
    (dotimes (i (length (window-list)))
      (goto-line line-number)
      (recenter-top-bottom)
      (other-window 1))))
(global-set-key (kbd "\C-c l") 'hao-same-line)

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

(defun hao-increment-number-at-point ()
  (interactive)
  (skip-chars-backward "-0123456789")
  (or (looking-at "-?[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun hao-decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "-0123456789")
  (or (looking-at "-?[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (- (string-to-number (match-string 0)) 1))))

;; Move line up and down
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; Duplicate current line
(defun hao-duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "M-i") 'hao-duplicate-line)

;; Toggle maximize buffer (I like it)
(defun hao-toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))
(global-set-key [(control x) (?1)] 'hao-toggle-maximize-buffer)

(defun hao-sort-lines-by-length (reverse beg end)
  "Sort lines by length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))

(defun hao-indent-whole-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key (kbd "C-x i") 'hao-indent-whole-buffer)


;; used in Ag
(defun hao-find-java-implementation ()
  (interactive)
  (ag-project-files (ag/dwim-at-point) (list :file-regex (ag/buffer-extension-regex)))
  (other-window 1))
(global-set-key (kbd "M-s j") 'hao-find-java-implementation)


;; pop local mark ring
(setq mark-ring-max 6)
(setq global-mark-ring-max 6)
(defun hao-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
"
  (interactive)
  (set-mark-command t))
(global-set-key (kbd "C-x C-x") 'hao-pop-local-mark-ring)

;; https://emacs.stackexchange.com/questions/2186/have-org-modes-exported-html-use-custom-id-when-linking-to-sub-sections-in-toc
(defun my/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point. If the entry does not
have an CUSTOM_ID, the function returns nil. However, when CREATE
is non nil, create a CUSTOM_ID if none is present already. PREFIX
will be passed through to `org-id-new'. In any case, the
CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new prefix))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun my/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the
current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (my/org-custom-id-get (point) 'create))))

;; automatically add ids to captured headlines
(add-hook 'org-capture-prepare-finalize-hook
          (lambda () (my/org-custom-id-get (point) 'create)))