#lang racket

; pass in thunk f. it won't get evaled yet
(define (delay y)
  (mcons #f y)
  )

; forcer in mcons
(define (force th)
  (cond [(mcar th) (mcdr th)]
        [#t (begin (set-mcar! th #t)
                   (set-mcdr! th (mcdr th))
                   (mcdr th))]))



; we'll calc y even if we don't need it
(define (mymult1 x y)
  (cond [(= x 0) 0]
        [(= x 1) y]
        [#t (+ y (mymult1 (- x 1) y))]))

; calc (y) x times
(define (mymult2 x yth)
  (cond [(= x 0) 0]
        [(= x 1) (yth)]
        [#t (+ (yth) (mymult2 (- x 1) yth))]))
                 

; pass in promise to delay execution
(mymult2 20 (force (delay (lambda () 90))))
;(mymult2 20 (let ([x (delay (lambda () 900))]) (lambda () (force x))))

;nuts
; get back an mcons with the thunk
; pass in the to-be cached thunk
; but the lambda is called right away, then cached
; better is to only called when needed

(mymult1 20 9)
(mymult1 9 20)
(mymult2 9 (lambda () 20))
(mymult2 20 (lambda () 9))