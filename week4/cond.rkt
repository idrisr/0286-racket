#lang racket

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (sum1 (cdr xs)))
      )
  )

(define (sum2 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum2 (cdr xs)))]
        [#t (sum2 (cdr xs))]))
                    
(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [(list? (car xs)) (+ (sum3 (car xs)) (sum3 (cdr xs)))]
        [#t (sum3 (cdr xs))]))


(define l '(1 2 3 (1 2 60) "idris"))
(sum1 l)
(sum2 l)
(sum3 l)