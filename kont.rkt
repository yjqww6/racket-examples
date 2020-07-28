#lang racket/base
(require racket/stream (for-syntax racket/base syntax/parse))

(provide (all-defined-out))

(define ((list-k ls) k)
  (for-each k ls))

(define ((range-k n) k)
  (for ([i (in-range n)])
    (k i)))

(struct kont-stream (v [r #:mutable])
  #:methods gen:stream
  [(define (stream-empty? self) #f)
   (define (stream-first self) (kont-stream-v self))
   (define (stream-rest self) ((kont-stream-r self)))])

(define (in-kont/proc f [tag (default-continuation-prompt-tag)])
  (call-with-continuation-prompt
   (λ ()
     (f (λ (v) (call-with-composable-continuation
                (λ (k)
                  (abort-current-continuation
                   tag
                   (λ ()
                     (define stream
                       (kont-stream
                        v
                        (λ ()
                          (define r (call-with-continuation-prompt
                                     k
                                     tag (λ (thunk) (thunk))))
                          (set-kont-stream-r! stream (λ () r))
                          r)))
                     stream)))
                tag)))
     empty-stream)
   tag
   (λ (thunk) (thunk))))

(define-sequence-syntax in-kont
  (λ () #'in-kont/proc)
  (λ (stx)
    (syntax-parse stx
      [[(x:id ...)
        (_ f:expr
           (~optional t:expr #:defaults
                      ([t #'(default-continuation-prompt-tag)])))]
       #:with (tmp ...) (generate-temporaries #'(x ...))
       #:with (falsy ...) (map (λ (_) #'#f) (syntax->list #'(x ...)))
       #'[(k x ...)
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
           ([(k x ...) (loopme)])
           #t
           #t
           ((k)))]])))