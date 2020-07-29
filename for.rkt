#lang racket/base

(require racket/stream
         (for-syntax racket/base syntax/parse racket/list
                     syntax/unsafe/for-transform))
(provide in-mapped in-filtered)

(define pre (gensym))

(define-sequence-syntax in-mapped
  (λ () #'stream-map)
  (λ (stx)
    (define (compose acc o1 s1)
      (syntax-case acc ()
        [(([(outer-id0 ...) outer-expr0] ...)
          outer-check0
          ([loop-id0 loop-expr0] ...)
          pos-guard0
          ([(inner-id0 ...) inner-expr0] ...)
          pre-guard0
          post-guard0
          (loop-arg0 ...))
         (syntax-case (expand-for-clause o1 s1) ()
           [(([(outer-id1 ...) outer-expr1] ...)
             outer-check1
             ([loop-id1 loop-expr1] ...)
             pos-guard1
             ([(inner-id1 ...) inner-expr1] ...)
             pre-guard1
             post-guard1
             (loop-arg1 ...))
            #'(([(outer-id1 ...) outer-expr1] ...
                [(outer-id0 ...) outer-expr0] ...)
               (begin outer-check1 outer-check0)
               ([loop-id1 loop-expr1] ... [loop-id0 loop-expr0] ...)
               (and pos-guard1 pos-guard0)
               ([(inner-id1 ...) inner-expr1] ...
                [(inner-id0 ...) inner-expr0] ...)
               (and pre-guard1 pre-guard0)
               (and post-guard1 post-guard0)
               (loop-arg1 ... loop-arg0 ...))])]))
    
    (syntax-parse stx
      [[(Id:id) (_ Proc:expr S:expr S*:expr ...)]
       #:with (Temp ...) (generate-temporaries #'(S* ...))
       (syntax-parse (for/fold ([s (expand-for-clause #'S #'[(temp-id) S])])
                               ([s1 (in-list (syntax->list #'([(Temp) S*] ...)))]
                                [o1 (in-list (syntax->list #'(S* ...)))])
                       (compose s o1 s1))
         [(([(outer-id ...) outer-expr] ...)
           outer-check
           ([loop-id loop-expr] ...)
           pos-guard
           ([(inner-id ...) inner-expr] ...)
           pre-guard
           post-guard
           (loop-arg ...))
          #:with (falsy ...) (map (λ (_) #'#f)
                                  (syntax->list #'(inner-id ... ...)))
          #'[(Id)
             (:do-in
              ([(outer-id ...) outer-expr] ... [(proc) Proc])
              outer-check
              ([loop-id loop-expr] ...)
              pos-guard
              ([(Id inner-id ... ...) (let-values ([(inner-id ...) inner-expr] ...)
                                        (if pre-guard
                                            (values (proc temp-id Temp ...) inner-id ... ...)
                                            (values pre falsy ...)))])
              (not (eq? Id pre))
              post-guard
              (loop-arg ...)
              )]])])))

(define-sequence-syntax in-filtered
  (λ () #'stream-filter)
  (λ (stx)
    (syntax-case stx ()
      [[(Id) (_ Proc S)]
       (syntax-parse (expand-for-clause #'S #'[(Id) S])
         [(([(outer-id ...) outer-expr] ...)
           outer-check
           ([loop-id loop-expr] ...)
           pos-guard
           ([(inner-id ...) inner-expr] ...)
           pre-guard
           post-guard
           (loop-arg ...))
          
          #:with (falsy ...) (map (λ (_) #'#f)
                                  (syntax->list #'(inner-id ... ... loop-id ...)))
          #:with (inner-loop-id ...) (remove-duplicates (syntax->list #'(inner-id ... ... loop-id ...))
                                                        bound-identifier=?)
          
          #'[(Id)
             (:do-in
              
              ([(next outer-id ... ...)
                (let-values ([(outer-id ...) outer-expr] ...
                             [(proc) Proc])
                  (define (next loop-id ...)
                    (if pos-guard
                        (let-values ([(inner-id ...) inner-expr] ...)
                          (if pre-guard
                              (if (proc Id)
                                  (values #t inner-loop-id ...)
                                  (if post-guard
                                      (next loop-arg ...)
                                      (values #f falsy ...)))
                              (values #f falsy ...)))
                        (values #f falsy ...)))
                  outer-check
                  (values next outer-id ... ...))])

              (void)

              ([loop-id loop-expr] ...)

              #t

              ([(ok inner-loop-id ...) (next loop-id ...)])

              ok

              post-guard

              (loop-arg ...)
              )]])])))


(module+ main
  (require racket/list)
  (time
   (for ([_ (in-range 100)])
     (for/and ([i (in-mapped add1 (in-filtered even? (in-range 100000)))]
               [j (in-filtered odd? (in-range 100000))])
       (= i j))))
  (time (for ([_ (in-range 100)])
          (andmap =
                  (map add1 (filter even? (range 100000)))
                  (filter odd? (range 100000)))))
  (time (for ([_ (in-range 100)])
          (for/sum ([i (in-mapped + (in-mapped add1 (in-filtered even? (in-range 100000)))
                                  (in-filtered odd? (in-range 100000))
                                  (in-mapped sub1 (in-filtered odd? (in-range 100000))))])
            i)))
  (time (for ([_ (in-range 100)])
          (foldl + 0 (map +
                          (map add1 (filter even? (range 100000)))
                          (map sub1 (filter odd? (range 100000))))))))