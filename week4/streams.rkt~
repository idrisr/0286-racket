#lang racket
; -1) stream is a thunk
; 0) that produces a pair
; 1) first element in the sequence
; 2) thunk the reps stream for second through infinity elements


; function that creates a pair, car of element, cdr of next element
; start with function that creates seed

; this has got to be SUCC (SB). f(x, g(x))--- or is this (x, f(g(x))
; it must be SUCC (SB), which simplifies to 

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (twototh)
  (define (f x)
    (cons x (lambda () (f (* x 2))))); (x, g(f(x))
  (f 2))

(define (ones)
  (define (f)
    (cons 1 ones))
  (f)
  )


; take in a stream and a tester/predicate
; locally bound function f takes in stream and ans
; bind (stream) to avoid re-calcing
; test predicate
; true: ans
; false: f (rest of stream) (ans + 1)
; kick off f

(define (number-until stream tester)
  (define (f stream ans)
    (let ([pr (stream)])
      (if (tester (car pr))
          ans
          (f (cdr pr) (+ ans 1)))))
  (f stream 1))


;(define onez (stream-maker (lambda (x y) 1) 1))
;(define natz (stream-maker + 1))
;(define twoz (stream-maker * 2))

;(define s twoz)
(car (s))
(car ((cdr (s))))
(car ((cdr ((cdr (s))))))
(car ((cdr ((cdr ((cdr (s))))))))
(car ((cdr ((cdr ((cdr ((cdr (s))))))))))

(number-until twoz (lambda (x) (= x (* 1024 1024 1024 1024 1024))))
