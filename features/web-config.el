;; web-mode
;; jumping: C-c C-n
;; folding: C-c C-f
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'")
        ("blade"  . "\\.blade\\."))
)