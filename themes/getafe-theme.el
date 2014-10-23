;;; getafe-theme.el --- Yet another getafe theme for Emacs 24

;; Copyright (C) 2013 Huang Bin

;; Author: Huang Bin <embrace.hbin@gmail.com>
;; URL: https://github.com/hbin/getafe-theme
;; Version: 0.8

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is another getafe dark theme for Emacs 24.
;; Equiped with my favorites.

;;; Requirements:
;;
;; Emacs 24

;;; Code:
(deftheme getafe "The getafe color theme for Emacs 24")

(let ((class '((class color) (min-colors 89)))
      ;; getafe palette
      (getafe-white          "#f8fff9")
      (getafe-black          "#000000")
      (getafe-cyan           "#69c3ff")
      (getafe-brown          "#1b1d1e")
      (getafe-fg             "#f8fff9")
      (getafe-red            "#ff0000")
      (getafe-pink           "#ff358b")
      (getafe-orange+5       "#ef5939")
      (getafe-orange         "#dddd66")
      (getafe-yellow         "#ffdc00")
      (getafe-darkgoldenrod  "#ffa042")
      (getafe-wheat          "#c4be89")
      (getafe-olive          "#808000")
      (getafe-chartreuse     "#f8fff9")
      (getafe-lime           "#00ff00")
      (getafe-green          "#aeee00")
      (getafe-darkwine       "#1e0010")
      (getafe-maroon         "#800000")
      (getafe-wine           "#960050")
      (getafe-teal           "#008080")
      (getafe-aqua           "#00ffff")
      (getafe-blue           "#01b0f0")
      (getafe-slateblue      "#7070f0")
      (getafe-purple         "#b994ff")
      (getafe-palevioletred  "#d33682")
      (getafe-grey-2         "#bcbcbc")
      (getafe-grey-1         "#8f8f8f")
      (getafe-grey           "#5a7085")
      (getafe-grey+2         "#403d3d")
      (getafe-grey+3         "#4c4745")
      (getafe-grey+5         "#232526")
      (getafe-bg             "#1b1d1e")
      (getafe-grey+10        "#080808")
      (getafe-dark           "#000000")
      (getafe-base01         "#465457")
      (getafe-base02         "#455354")
      (getafe-base03         "#293739")
      (getafe-dodgerblue     "#13354a"))
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
   `(font-lock-constant-face ((t (:foreground ,getafe-purple))))
   `(font-lock-doc-string-face ((t (:foreground ,getafe-darkgoldenrod))))
   `(font-lock-function-name-face ((t (:foreground ,getafe-chartreuse))))
   `(font-lock-keyword-face ((t (:foreground ,getafe-pink :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,getafe-wine))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,getafe-darkgoldenrod))))
   `(font-lock-type-face ((t (:foreground ,getafe-blue :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,getafe-orange))))
   `(font-lock-warning-face ((t (:foreground ,getafe-palevioletred :weight bold))))

   ;; mode line
   `(mode-line ((t (:foreground ,getafe-fg
                                :background ,getafe-base03
                                :box nil))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-inactive ((t (:foreground ,getafe-fg
                                         :background ,getafe-base02
                                         :box nil))))

   ;; search
   `(isearch ((t (:foreground ,getafe-dark :background ,getafe-wheat :weight bold))))
   `(isearch-fail ((t (:foreground ,getafe-wine :background ,getafe-darkwine))))

   ;; linum-mode
   `(linum ((t (:foreground ,getafe-grey-2 :background ,getafe-grey+5))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,getafe-grey+5)) (t :weight bold)))
   `(hl-line ((,class (:background ,getafe-grey+5)) (t :weight bold)))

   ;; TODO
   ;; ido-mode
   ;; flycheck
   ;; show-paren
   ;; rainbow-delimiters
   ;; highlight-symbols
   ))

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

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; getafe-theme.el ends here
