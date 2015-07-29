;; Step1: M-x sql-mysql ;; Create an interactive shell
;; Step2: Open a buffer in sql mode
;;        M-x sql-set-product
;;        M-x sql-set-sqli-buffer
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))


(setq sql-connection-alist
      '((pool-a
         (sql-product 'mysql)
         (sql-server "10.94.65.102")
         (sql-user "root")
         (sql-password "123456")
         (sql-database "istudy")
         (sql-default-character-set "utf8")
         (sql-port 8306))
        (pool-b
         (sql-product 'mysql)
         (sql-server "ip2")
         (sql-user "user2")
         (sql-password "pwd2")
         (sql-database "db2")
         (sql-port 3306))))

(defun sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (eval `(let ,(cdr (assoc name sql-connection-alist))
    (flet ((sql-get-login (&rest what)))
      (sql-product-interactive sql-product)))))

(defun hao-mysql-a ()
  (interactive)
  (sql-connect-preset 'pool-a))

(defun hao-mysql-b ()
  (interactive)
  (sql-connect-preset 'pool-b))

(defun my-sql-connect (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))