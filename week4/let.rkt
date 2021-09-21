#lang racket

; later variables do not have access to prior ones - no recursion
(define (silly-double x)
  (let ([x (+ x 3)]
        (y (+ x 2)))
    (+ x y -5)))

; later variables do have access to prior ones - no recursion
(define (silly-double2 x)
  (let* ([x (+ x 3)]
         [y (+ x 2)])
    (+ x y -8)))

; recursion
(define (triple x)
  (letrec ([y (+ x 2)]
           [f (lambda (z) (+ z y w x))]
           [w (+ x 7)])
    (f -9)))


(silly-double 9)
(silly-double2 9)
(triple 9)