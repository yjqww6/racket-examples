#lang racket/base

(require ffi/unsafe ffi/unsafe/port)
(provide sendfile sendfile*)
    
(define (wait socket-port)
  (semaphore-wait (unsafe-socket->semaphore (unsafe-port->socket socket-port)
                                            'write)))

(define sendfile
  (let ([send (get-ffi-obj
               "sendfile" #f
               (_fun
                #:save-errno 'posix
                _int _int (_box _int64) _size
                -> _ssize))])
    (define (port-size file-port)
      (file-position file-port eof)
      (begin0
        (file-position file-port)
        (file-position file-port 0)))
    
    (λ (path socket-port)
      (call-with-input-file path
        (λ (file-port)
          (define size (port-size file-port))
          (define off (box 0))
          (begin0
            (let loop ()
              (wait socket-port)
              (define ret
                (send (unsafe-port->socket socket-port)
                      (unsafe-port->file-descriptor file-port)
                      off
                      (- size (unbox off))))
              (cond
                [(= ret -1)
                 (raise (exn:fail:network:errno
                         "sendfile failed"
                         (current-continuation-marks)
                         (saved-errno)))]
                [(or (= (unbox off) size) (= ret 0))
                 (unbox off)]
                [else (loop)]))
            (wait socket-port)))))
    
    ))

(define sendfile*
  (let ([send (get-ffi-obj
               "sendfile" #f
               (_fun
                #:save-errno 'posix
                _int _int (_intptr = 0) _size
                -> _ssize))])

    (λ (path socket-port)
      (call-with-input-file path
        (λ (file-port)
          (begin0
            (let loop ([size 0])
              (wait socket-port)
              (define ret
                (send (unsafe-port->socket socket-port)
                      (unsafe-port->file-descriptor file-port)
                      4096))
              (cond
                [(= ret -1)
                 (raise (exn:fail:network:errno
                         "sendfile failed"
                         (current-continuation-marks)
                         (saved-errno)))]
                [(= ret 0) size]
                [else (loop (+ ret size))]))
            (wait socket-port)))))
    ))

(module+ main
  (require racket/tcp racket/port)
  (define (test a b)
    (define-values (i o) (tcp-connect "localhost" 8181))
    (close-input-port i)
    (begin0
      (sendfile a o)
      (sendfile* b o)
      (close-output-port o)))
  
  (define listener (tcp-listen 8181 4 #t))
  (define thr
    (thread
     (λ ()
       (define-values (i o) (tcp-accept listener))
       (close-output-port o)
       (displayln (port->bytes i))
       (close-input-port i))))
  (test (build-path (getenv "HOME") ".bashrc")
        "/sys/fs/cgroup/memory/memory.limit_in_bytes")
  (thread-wait thr))