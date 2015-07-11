;; Step1: M-x sql-mysql ;; Create an interactive shell
;; Step2: Open a buffer in sql mode
;;        M-x sql-set-product
;;        M-x sql-set-sqli-buffer
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))