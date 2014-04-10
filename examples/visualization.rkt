#lang racket

(require mosaic/model
         mosaic/interface
         mosaic/xml
         mosaic/visualization
         plot)

(define (read-configurations in)
  (for/list ([item (items-from-xml in)]
             #:when (configuration? (cdr item)))
    (make-data-item (cdr item))))

(define conf
  (first (call-with-input-file "../mosaic/tests/water.xml" read-configurations)))

(define u (configuration.universe conf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (plot3d (sites-as-dots conf))

(plot3d (sites-as-colored-spheres conf) #:width 800 #:height 800)
