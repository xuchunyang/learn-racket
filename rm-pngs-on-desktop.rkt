;; rm ~/Desktop/*.png
#lang racket

(parameterize ([current-directory (expand-user-path "~/Desktop")])
  (for [(f (directory-list))]
    (when (and (file-exists? f)
               (string-suffix? (path->string f) ".png"))
      (delete-file f)
      (printf "removed '~a'\n" f))))
