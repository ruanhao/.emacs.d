;; -*- coding: utf-8 -*-
;; file  : getafe-theme.el
;; author: Hao Ruan
;; base  : https://github.com/hbin/molokai-theme
;; date  : 2014/1/26

;; Copyright (C) 2007 Free Software Foundation ,Inc. <http://fsf.org/>
;; Everyone is permitted to copy and distribute verbatim copies
;; of this license document ,but changing it is not allowed.

;; getafe color scheme for Emacs

(deftheme getafe "getafe color theme for Emacs")

(let ((class '((class color) (min-colors 89)))
      (getafe-rose "#ffe4e1")
      (getafe-gold "#ff500e")
      (getafe-white "#ffffff")
      (getafe-fg "#f8fff9")
      (getafe-red "#ff0000")
      (getafe-pink "#f92672")
      (getafe-orange+5 "#ef5939")
      (getafe-orange "#fd971f")
      (getafe-yellow "#ffdc00")
      (getafe-darkgoldenrod "#c6c5fe")
      (getafe-wheat "#c4be89")
      (getafe-olive "#808000")
      (getafe-chartreuse "#43c4f7")
      (getafe-lime "#00ff00")
      (getafe-green "#aeee00")
      (getafe-darkwine "#1e0010")
      (getafe-maroon "#800000")
      (getafe-wine "#960050")
      (getafe-teal "#008080")
      (getafe-aqua "#00ffff")
      (getafe-blue "#66d9ef")
      (getafe-slateblue "#7070f0")
      (getafe-purple "#ae81ff")
      (getafe-palevioletred "#d33682")
      (getafe-grey-2 "#bcbcbc")
      (getafe-grey-1 "#8f8f8f")
      (getafe-grey "#808080")
      (getafe-grey+2 "#403d3d")
      (getafe-grey+3 "#4c4745")
      (getafe-grey+5 "#232526")
      (getafe-bg "#1b1d1e")
      (getafe-grey+10 "#080808")
      (getafe-dark "#000000")
      (getafe-base01 "#465457")
      (getafe-base02 "#455354")
      (getafe-base03 "#293739")
      (getafe-dodgerblue "#13354a"))

  (custom-theme-set-faces
   'getafe

   ;; base
   `(default ((t (:background ,getafe-bg :foreground ,getafe-fg))))
   `(cursor ((t (:background ,getafe-fg :foreground ,getafe-bg))))
   `(fringe ((t (:foreground ,getafe-base02 :background ,getafe-bg))))
   `(highlight ((t (:background ,getafe-grey))))
   `(region ((t (:background ,getafe-grey+2))
             (t :inverse-video t)))
   `(warning ((t (:foreground ,getafe-palevioletred :weight bold))))

   ;; font lock
   `(font-lock-builtin-face ((t (:foreground ,getafe-chartreuse))))
   `(font-lock-comment-face ((t (:foreground ,getafe-base01))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,getafe-base01))))
   `(font-lock-constant-face ((t (:foreground ,getafe-yellow))))
   `(font-lock-doc-string-face ((t (:foreground ,getafe-darkgoldenrod))))
   `(font-lock-function-name-face ((t (:foreground ,getafe-chartreuse))))
   `(font-lock-keyword-face ((t (:foreground ,getafe-pink))))
   `(font-lock-negation-char-face ((t (:foreground ,getafe-wine))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,getafe-darkgoldenrod))))
   `(font-lock-type-face ((t (:foreground ,getafe-blue :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,getafe-green))))
   `(font-lock-warning-face ((t (:foreground ,getafe-palevioletred :weight bold))))

   ;; mode line
   `(mode-line ((t (:foreground ,getafe-fg
                                :background ,getafe-base03
                                :box nil))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-inactive ((t (:foreground ,getafe-fg
                                         :background ,getafe-base02
                                         :box nil))))

   ;; ediff
   '(ediff-current-diff-A ((t (:background "#383817"))))
   '(ediff-current-diff-B ((t (:inherit ediff-current-diff-A))))
   '(ediff-current-diff-C ((t (:background "#333333"))))
   '(ediff-even-diff-A ((t (:foreground "light gray" :background "#333333"))))
   '(ediff-even-diff-B ((t (:foreground "light gray" :background "#333333"))))
   '(ediff-even-diff-C ((t (:foreground "light gray" :background "#333333"))))
   '(ediff-fine-diff-A ((t (:foreground "navy" :background "sky blue"))))
   '(ediff-fine-diff-B ((t (:foreground "black" :background "cyan"))))
   ;;'(ediff-fine-diff-C ((t (:foreground "black" :background "Turquoise"))))
   '(ediff-odd-diff-A ((t (:foreground "light gray" :background "#333333"))))
   '(ediff-odd-diff-B ((t (:foreground "light gray" :background "#333333"))))
   '(ediff-odd-diff-C ((t (:foreground "light gray" :background "#333333"))))

   ;; search
   `(isearch ((t (:foreground ,getafe-dark :background ,getafe-wheat :weight bold))))
   `(isearch-fail ((t (:foreground ,getafe-wine :background ,getafe-darkwine))))

   ;; linum-mode
   `(linum ((t (:foreground ,getafe-grey-2 :background ,getafe-grey+5))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,getafe-grey+5)) (t :weight bold)))
   `(hl-line ((,class (:background ,getafe-grey+5)) (t :weight bold)))))

(defcustom getafe-theme-kit nil
  "Non-nil means load getafe-theme-kit UI component"
  :type 'boolean
  :group 'getafe-theme)

(defcustom getafe-theme-kit-file
  (concat (file-name-directory
           (or (buffer-file-name) load-file-name))
          "getafe-theme-kit.el")
  "getafe-theme-kit-file"
  :type 'string
  :group 'getafe-theme)

(if (and getafe-theme-kit
         (file-exists-p getafe-theme-kit-file))
    (load-file getafe-theme-kit-file))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'getafe)
