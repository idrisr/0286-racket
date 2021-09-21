#lang racket

(define (sequence low high stride)
  (range low (+ high 1) stride))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond  [(< n 0) (error "list-nth-mod: negative number")]
         [(= (length xs) 0) (error "list-nth-mod: empty list")]
         [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-maker g arg)
  (define (f x)
    (cons x (lambda () (f (g x arg)))))
  (lambda () (f arg)))

(define (stream-for-n-steps stream n)
  (define (f stream ans acc)
    (let ([pr (stream)])
      (if (= (length acc) n)
          (reverse acc)
          (f (cdr pr) (+ ans 1) (cons (car pr) acc)))))
  (f stream 1 '()))

(define twoz (stream-maker * 2))
(define funny-number-stream
  (stream-maker (lambda (x y)
                  (cond [(= 0 (remainder (+ 1 x) 5)) (* -1 (+ 1 x))]
                        [(< x 0) (+ 1 (* -1 x))]
                        [#t (+ x 1)]))
                1))

(define dan-then-dog
  (stream-maker (lambda (x y) (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")) "dan.jpg"))

(define (twototh)
  (define (f x)
    (cons x (lambda () (f (* x 2))))); (x, g(f(x))
  (f 2))

(define (stream-add-zero stream)
  (let ([pr (stream)])
    (lambda () (cons (cons 0 (car pr)) (lambda () ((stream-add-zero (cdr pr))))))))
  
(define (cycle-lists xs ys)
  (define (g n)
    (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (g (+ n 1)))))
  (lambda () (g 0))
  )

(define (vector-assoc v vec)
  (define (helper n)
    (if (= n (vector-length vec))
        #f
        (let ([val (vector-ref vec n)])
        (cond
          [(pair? val) (if (equal? (car val) v)
                           val
                           (helper (+ n 1)))]
          [#t (helper (+ n 1))]))))
  (helper 0))


(define xs (range 8))
(define ys '("idris" "raja"))

#|
(stream-for-n-steps funny-number-stream 20)
(stream-for-n-steps twoz 10)
(stream-for-n-steps dan-then-dog 5)
(stream-for-n-steps (stream-add-zero funny-number-stream) 16)
|#

(stream-for-n-steps (stream-add-zero (cycle-lists xs ys)) 6)
(define a (stream-for-n-steps (cycle-lists xs ys) 6))

#("a" "b" "c")
#(name (that tune))
#7(baldwin bruce)

(vector-assoc 3 #4(a b))
(vector-assoc 3 #4((cons 0 1)))
(define v (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)))
(vector-assoc 23 v)
