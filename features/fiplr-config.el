(require 'fiplr)
(setq fiplr-ignored-globs '((directories (".git" ".svn"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))
                            ;; NOTE: only-use-these is NOT IGNORED
                            (only-use-these ("*")))) ;; default is '*', means any file
(global-set-key (kbd "C-x f") 'fiplr-find-file)