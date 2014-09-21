        ;; hao-utils
(defun hao-show-system-type ()
  "Find out what OS Emacs is currently running on"
  (interactive)
  (message "%s" system-type))

(defun hao-show-features-loaded ()
  "Find out all features loaded"
  (interactive)
  (message "%S" features))

(defun hao-pick-word-at-point ()
  "Pick current word under cursor
this function would move cursor to the beginning of the word"
  (let (tail-point)
    (skip-chars-forward "-_A-Za-z0-9")
    (setq tail-point (point))
    (skip-chars-backward "-_A-Za-z0-9")
    (buffer-substring-no-properties (point) tail-point)))

(defun hao-pick-regexp-word-at-point ()
  "Pick current regexp word at point"
  (save-excursion
      (concat "\\b" (hao-pick-word-at-point) "\\b")))

(defun hao-highlight-word-at-point ()
  "Highlight the word at point"
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
  "Unhighlight all highlighted words"
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

;; Fill column indicator
(load-file "~/.emacs.d/fill-column-indicator.el")
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
(global-set-key [f7] 'hao-toggle-column-ruler)
