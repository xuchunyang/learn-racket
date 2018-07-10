#lang racket

(require net/url
         html-parsing
         sxml)

(define (expand-link target page)
  (define u (string->url page))
  (define scheme (url-scheme u))
  (define host (url-host u))
  (define base
    (let ([filename (file-name-from-path page)])
      (if filename
          (substring page 0 (- (string-length page)
                               (string-length (path->string filename))))
          page)))
  (cond [(or (string-prefix? target "http://")
             (string-prefix? target "https://"))
         target]
        [(string-prefix? target "//")
         (string-append scheme ":" target)]
        [(string-prefix? target "/")
         (string-append scheme "://" host target)]
        [else
         (string-append base target)]))

(module+ test
  (require rackunit)
  (define page "https://www.gnu.org/software/emacs/tour/index.html")
  (check-equal?
   (expand-link "/graphics/heckert_gnu.transp.small.png" page)
   "https://www.gnu.org/graphics/heckert_gnu.transp.small.png")
  (check-equal?
   (expand-link "images/splash-small.png" page)
   "https://www.gnu.org/software/emacs/tour/images/splash-small.png")
  (check-equal?
   "https://www.gnu.org/software/emacs/tour/images/splash-small.png"
   (expand-link "https://www.gnu.org/software/emacs/tour/images/splash-small.png"
               page)))

(define (extract-image-urls page)
  (printf "Downloading ~a...\n" page)
  (define-values (status headers html) (http-sendrecv/url (string->url page)))
  (define xexp (html->xexp html))
  (for/list ([target (map cadr ((sxpath "//img/@src") xexp))]
             #:when (non-empty-string? target))
    (expand-link target page)))

(define (download-image imgurl)
  (printf "Downloading ~a...\n" imgurl)
  (define filename (file-name-from-path imgurl))
  (unless (file-exists? filename)
    (define-values (status headers in) (http-sendrecv/url (string->url imgurl)))
    (call-with-output-file filename #:exists 'truncate
      (lambda (out)
        (copy-port in out)))))

(define (download-images page)
  (for-each download-image (extract-image-urls page)))

(module+ main
  (define page (vector-ref (current-command-line-arguments) 0))
  (download-images page))
