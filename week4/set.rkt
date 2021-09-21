#lang racket
(define b 3)
(define f (lambda (x) (* x b))) ; not evaled until called
(define c (+ b 4))
(set! b 5)
(define z (f 4))
(define w c)