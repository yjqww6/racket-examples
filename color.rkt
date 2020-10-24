#lang racket
(require xml syntax-color/module-lexer)

(define (types in)
  (let loop ([mode #f] [s '()])
    (define-values (str type _1 start end _4 new-mode)
      (module-lexer in 0 mode))
    (cond
      [(eof-object? str) (reverse s)]
      [(memq type
             '(comment sexp-comment constant
                       string parenthesis hash-colon-keyword symbol))
       (loop new-mode (cons (vector (sub1 start) (sub1 end) type str) s))]
      [else (loop new-mode s)])))

(define (colorme bstr)
  (define colored
    (types (open-input-bytes bstr)))

  (let loop ([pos 0] [colored colored])
    (match colored
      [(cons (vector start end type s) rest)
       (cond
         [(< pos start)
          (define bs (subbytes bstr pos start))
          (cons `(code ((class "racket")) ,(bytes->string/utf-8 bs))
                (loop start colored))]
         [(= pos start)
          (writeln s)
          (cons `(code ((class ,(string-append "racket "
                                               (symbol->string type))))
                       ,s)
                (loop end rest))])]
      [_
       (cond
         [(>= pos (bytes-length bstr))
          '()]
         [else
          (list `(code ((class "racket")) ,(bytes->string/utf-8 (subbytes bstr pos))))])])))

(define style
  #<<style
code.racket {
  font-family: 'Fira-Mono', monospace;
}
code.parenthesis {
  color: rgb(132,60,36);
}
code.symbol {
  color: rgb(38, 38, 128);
}
code.hash-colon-keyword {
  color: rgb(132, 60, 36);
}
code.string,code.constant {
  color: rgb(41, 128, 38);
}
code.comment,code.sexp-comment {
  color: rgb(192, 116, 31);
}
style
  )

(module+ main
  (require racket/runtime-path)
  (define file (path-add-extension (make-temporary-file) ".html"))
  (define-runtime-path color.rkt "color.rkt")
  (display-to-file
   (xexpr->string
    `(html () (head ()
                    (meta ((charset "utf8")))
                    (style () ,style))
           (body ()
                 (pre () ,@(colorme (file->bytes color.rkt))))))
   file
   #:exists 'replace)
  
  (require net/sendurl)
  (void (λ () (void)))
  (send-url/file (path->string file)))