;; Step1: M-x sql-mysql
;; Step2: Goto *.sql file buffer
;;        M-x sqlset-product
;;        M-x sql-set-sqli-buffer
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))