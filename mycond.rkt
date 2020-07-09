#lang racket/base

(require (for-syntax racket/base syntax/parse racket/syntax
                     racket/control))

(provide mycond)

(begin-for-syntax
  (define tag (make-continuation-prompt-tag))
  (define-syntax-rule (define-clauses name k [cl expe] ...)
    (define-splicing-syntax-class name
      #:attributes (exp)
      (pattern cl
               #:attr exp
               (λ () (shift-at tag k #`expe)))
      ...))

  (define-clauses cl k
    [(~seq #:else ~! body:expr ...+)
     (let () body ...)]
    [(~seq #:~> ~! (form ...))
     (form ... #,(k))]
    [(~seq #:do ~! [def:expr ...])
     (let () def ... #,(k))]
    [(~seq #:shifted-stx! ~! ks:id form:expr)
     (let-syntax ([ks (let ([s #'#,(k)])
                        (λ (stx) s))])
       form)]
    [(~seq #:ec ~! (succ:id fail:id) clause:cl)
     (let/ec succ
       (let/ec fail
         (succ #,(reset-at tag ((attribute clause.exp))
                           #'(fail))))
       #,(k))]
    
    [[test:expr #:=> proc:expr]
     (let ([tmp test])
       (if tmp
           (proc tmp)
           #,(k)))]

    [[test:expr #:as it:id body:expr ...+]
     (let ([it test])
       (if it
           (let () body ...)
           #,(k)))]
    
    [[test:expr body:expr ...+]
     (if test
         (let () body ...)
         #,(k))]))

(define-syntax (mycond stx)
  (syntax-parse stx
    [(_ clauses:cl ...+)
     (reset-at tag
               (for-each (λ (x) (x)) (attribute clauses.exp))
               (raise-syntax-error 'mycond "#:else not found" stx))]))

(module+ test
  (require racket/match rackunit)

  (test-case
   "else"
   (check-equal? (mycond [#f 0] #:else 2 1) 1))

  (test-case
   "test"
   (check-equal? (mycond [#f 0]
                         [1 #:=> add1]
                         #:else (void))
                 2)
   (check-equal? (mycond [#f 0]
                         [1 #:=> add1]
                         #:else (void))
                 2)

   (check-equal? (mycond [#f 0]
                         [1 #:as me (add1 me)]
                         #:else (void))
                 2)
   )

  (test-case
   "threading"
   (check-equal? (mycond
                  #:~> (let ([f 1]))
                  #:else
                  f)
                 1)
   (check-equal? (mycond
                  #:~> (match-let ([(cons f f) '(1 . 1)]))
                  #:else
                  f)
                 1)
   (check-equal? (mycond
                  #:~> (let/ec k)
                  #:else
                  (+ "1" (k 1)))
                 1))

  (test-case
   "shifted-stx!"
   (check-equal? (mycond #:shifted-stx! k
                         (match #f
                           [(cons f f) 9]
                           [_ k])
                         [#t 1]
                         #:else 0)
                 1))

  (test-case
   "ec"
   (check-equal? (mycond #:ec (s f) [#t (f)]
                         #:else 0)
                 0)
   (check-equal? (mycond #:ec (s f) [#f (error 't)]
                         #:else 0)
                 0)
   (check-equal? (mycond #:ec (s f) [#t 1]
                         #:else 0)
                 1)
   (check-equal? (mycond #:ec (s f)
                         #:do [(match '(1 . 1)
                                 [(cons f f) (s 9)]
                                 [_ (f)])]
                         [#t 1]
                         #:else 1)
                 9))
  )