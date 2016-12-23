;; PlantUml is good
;; Prerequisite:
;; 1. download plantuml.jar
;; 2. brew install graphviz

;; Template:

;; #+begin_src plantuml :file tryout.png
;;   Alice -> Bob: synchronous call
;;   Alice ->> Bob: asynchronous call
;; #+end_src

;; Try C-c C-e h h (org-html-export-to-html)
;; Or just C-c C-c on the SRC paragraph to generate png

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '( ;; other Babel languages
   (plantuml . t)))
(setq org-plantuml-jar-path
      (expand-file-name "~/plantuml.jar"))
