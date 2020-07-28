#lang racket/base
(require racket/stream (for-syntax racket/base syntax/parse))

(provide (all-defined-out))

(define ((list-k ls) k)
  (for-each k ls))

(define ((range-k n) k)
  (for ([i (in-range n)])
    (k i)))

(define (in-kont/proc f [tag (default-continuation-prompt-tag)])
  (call-with-continuation-prompt
   (λ ()
     (f (λ (v) (call-with-composable-continuation
                (λ (k)
                  (abort-current-continuation
                   tag
                   (λ () (stream-cons v (call-with-continuation-prompt
                                         k
                                         tag (λ (thunk) (thunk)))))))
                tag)))
     empty-stream)
   tag
   (λ (thunk) (thunk))))

(define-sequence-syntax in-kont
  (λ () #'in-kont/proc)
  (λ (stx)
    (syntax-parse stx
      [[(~or* (v:id ...) x:id)
        (_ f:expr
           (~optional t:expr #:defaults
                      ([t #'(default-continuation-prompt-tag)])))]
       #:with (param ...) (if (attribute x) #'(x) #'(v ...))
       #:with (tmp ...) (generate-temporaries #'(param ...))
       #:with (falsy ...) (map (λ (_) #'#f) (syntax->list #'(param ...)))
       #'[(k param ...)
          (:do-in
           ([(proc) (let ([tag t]
                          [proc f])
                      (call-with-continuation-prompt
                       (λ ()
                         (proc
                          (λ (tmp ...)
                            (call-with-composable-continuation
                             (λ (k)
                               (abort-current-continuation
                                tag
                                (λ () (λ () (values
                                             (λ ()
                                               (call-with-continuation-prompt
                                                k
                                                tag (λ (thunk) (thunk))))
                                             tmp ...)))))
                             tag)))
                         #f)
                       tag
                       (λ (thunk) (thunk))))])
           (void)
           ([loopme proc])
           loopme
           ([(k param ...) (loopme)])
           #t
           #t
           ((k)))]])))