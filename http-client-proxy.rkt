#lang racket/base
;;;; This shows how to use unix-socket and proxy for net/http-client

(require racket/port net/http-client)

(module socks5 racket/base
  (require racket/tcp racket/match)
  (provide (all-defined-out))

  (define (check-ip host)
    (cond
      [(regexp-match #rx"^(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\.(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\.(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\.(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])$" host)
       =>
       (λ (ls) (list->bytes (map string->number (cdr ls))))]
      [else #f]))
  
  (define ((socks5-connect host port) rhost rport)

    (define-values (i o) (tcp-connect host port))
    (write-bytes #"\5\1\0" o)
    (flush-output o)
    (match (read-bytes 2 i)
      [#"\5\0" (void)])

    (write-bytes #"\5\1\0" o)
    (cond
      [(check-ip rhost)
       =>
       (λ (b)
         (write-byte 1 o)
         (write-bytes b o))]
      [else
       (write-byte 3 o)
       (define b (string->bytes/utf-8 rhost))
       (write-byte (bytes-length b) o)
       (write-bytes b o)])
    (write-bytes (integer->integer-bytes rport 2 #f #t) o)
    (flush-output o)
    
    (match (read-bytes 10 i)
      [#"\5\0\0\1\0\0\0\0\0\0" (values i o)]
      [else (error 'socks5-connect)]))
  )

(module+ test-unix-socket
  (require racket/unix-socket)
  (define-values (i o) (unix-socket-connect "/var/run/docker.sock"))

  (define hc (http-conn-open "" #:ssl? (list #f i o close-output-port)))

  (define-values (status header port)
    (http-conn-sendrecv! hc "/images/json"))
  (displayln (port->bytes port))
  (http-conn-close! hc))

(module+ test-socks5
  (require (submod ".." socks5) openssl racket/tcp)
  (define-values (i o) ((socks5-connect "localhost" 10081) "github.com" 443))
  (define ctx (ssl-make-client-context))
  (define-values (f t) (ports->ssl-ports i o #:mode 'connect #:hostname "github.com"
                                         #:context ctx))
  (define hc (http-conn-open "github.com"
                             #:ssl? (list ctx f t ssl-abandon-port)
                             #:port 443))
  (define-values (status header port) (http-conn-sendrecv! hc "/"))
  (displayln (port->bytes port))
  (http-conn-close! hc))