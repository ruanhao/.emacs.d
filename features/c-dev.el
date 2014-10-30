(defun hao-only-one-cscope ()
  "Leave only current window and the window which contains buffer *cscope*"
  (interactive)  ;; No need to save excursion
  (let ((origin-window (selected-window)))
    (mapc (lambda (window)
            (or (equal (buffer-name (window-buffer window)) "*cscope*")
                (eq origin-window window)
                (delete-window window)))
          (window-list))))

(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (c-set-style "linux")
             (flyspell-prog-mode)
	     (setq indent-tabs-mode nil)
	     (setq c-basic-offset 4)
             (modify-syntax-entry ?_ "w")
             ;; cscope setup
             (load-file "~/.emacs.d/cscope/contrib/xcscope/xcscope.el")
             (setq cscope-do-not-update-database t)
             (global-set-key (kbd "M-.") 'cscope-find-global-definition-no-prompting)
             (global-set-key (kbd "M-,") 'cscope-pop-mark)
             ;; (global-set-key (kbd "\C-x 1") 'hao-only-one-cscope)
             ))
