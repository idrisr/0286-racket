#lang racket

(define x (cons 14 null))
(define y x)
(set! x (cons 42 null))

x
y