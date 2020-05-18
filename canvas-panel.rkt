#lang racket/base
(require racket/class
         mred/private/lock
         mred/private/const
         mred/private/check
         mred/private/wx
         mred/private/wxpanel
         mred/private/mrwindow
         mred/private/mrcontainer
         mred/private/helper
         mred/private/mrcanvas)

(define default-paint-cb (λ (canvas dc) (void)))

(define canvas-panel%
  (class* (make-subwindow%
           (make-area-container-window%
            (make-window% #f (make-subarea% (make-container% area%)))))
    (canvas<%> area-container-window<%>)
    (init parent
          [style null]
          [paint-callback default-paint-cb]
          [enabled #t]
          [vert-margin no-val]
          [horiz-margin no-val]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (define wx #f)
    (public* [get-initial-label (lambda () #f)])
    (define paint-cb paint-callback)

    (define paintme (λ () (on-paint)))
    (define on-tab-in-me (λ () (on-tab-in)))

    (define/public-final (on-char e) (void))
    (define/public-final (on-event e) (void))
    (define/public (on-paint) (paint-cb this (get-dc)))
    (define/public-final (on-tab-in) (void))
    
    (define min-client-width (param (lambda () wx) min-client-width))
    (define min-client-height (param (lambda () wx) min-client-height))
    (public min-client-width min-client-height)
    
    (define/public (get-scaled-client-size)
      (send wx get-scaled-client-size))
    (define/public (set-resize-corner on?)
      (send wx set-resize-corner on?))
    
    (define/public (get-canvas-background) #f)
    (define/public (set-canvas-background color)
      (raise (exn:fail:contract "canvas-panel% is transparent" (current-continuation-marks))))
    
    (define/public (suspend-flush)
      (send wx begin-refresh-sequence))
    (define/public (resume-flush)
      (send wx end-refresh-sequence))
    (define/public (flush) (send wx flush))
    
    (define/public (get-dc) (send wx get-dc))
    
    (let* ([who 'panel]
           [cwho `(constructor ,who)])
      (check-container-parent cwho parent)
      (check-style cwho #f '(border deleted
                                    hscroll vscroll
                                    auto-hscroll auto-vscroll 
                                    hide-hscroll hide-vscroll)
                   style)

      (define (add-scrolls style)
        (define as-canvas?
          (or (memq 'vscroll style)
              (memq 'auto-vscroll style)
              (memq 'hide-vscroll style)
              (memq 'hscroll style)
              (memq 'auto-hscroll style)
              (memq 'hide-hscroll style)))
        (let ([style (if as-canvas? style
                         (append '(hide-vscroll hide-hscroll) style))])
          (append
           (if (memq 'hide-vscroll style) 
               '(auto-vscroll)
               null)
           (if (memq 'hide-hscroll style) 
               '(auto-hscroll)
               null)
           style)))

      (as-entry
       (lambda ()
         (super-instantiate
             ((lambda () (set! wx (make-object (class wx-canvas-panel%
                                                 (define/override (on-paint)
                                                   (paintme))
                                                 (define/public (on-tab-in)
                                                   (on-tab-in-me))
                                                 (super-new))
                                    this this (mred->wx-container parent)
                                    (cons 'transparent (add-scrolls style))
                                    (get-initial-label)))
                wx)
              (lambda () wx)
              (lambda () wx)
              (lambda () (check-container-ready cwho parent))
              #f parent #f)
           [enabled enabled]
           [vert-margin vert-margin]
           [horiz-margin horiz-margin]
           [border border]
           [spacing spacing]
           [alignment alignment]
           [min-width min-width]
           [min-height min-height]
           [stretchable-width stretchable-width]
           [stretchable-height stretchable-height])
         (unless (memq 'deleted style)
           (send (send wx area-parent) add-child wx))))
      (send parent after-new-child this))))

(module+ main
  (require racket/gui/base)
  (define a (new frame% [label "test"] [width 640] [height 480]))
  (define p (new canvas-panel%
                 [parent a]
                 [min-height 100]
                 [paint-callback
                  (λ (c dc)
                    (send dc set-background "gray")
                    (send dc clear)
                    (send dc set-smoothing 'smoothed)
                    (define-values (w h) (send dc get-size))
                    (send dc set-brush "lightgray" 'solid)
                    (send dc draw-ellipse
                          0 0 w h))]))
  (define p1 (new horizontal-panel% [parent p]))
  (void (new text-field% [parent p1] [label "input1"]))
  (void (new text-field% [parent p1] [label "input2"]))
  (send a show #t))
