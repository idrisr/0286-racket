#lang racket

(define (sequence low high stride)
  (range low (+ high 1) stride))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond  [(< n 0) (error "list-nth-mod: negative number")]
         [(= (length xs) 0) (error "list-nth-mod: empty list")]
         [#t (car (list-tail xs (remainder (length xs) n)))]))

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

; Write a function stream-add-zero that takes a stream s and returns another stream.
; If s would produce v for its ith element,
; then (stream-add-zero s) would produce the pair (0 . v) for its ith element.
; Sample solution: 4 lines. Hint: Use a thunk that when called uses s and recursion.
; Note: One of the provided tests in the file using graphics uses (stream-add-zero dan-then-dog) with place-repeatedly
(define (stream-add-zero stream)
  (let ([pr (stream)])
    (cons (cons 0 (car pr)) (lambda () (stream-add-zero (cdr pr))))))
  

(stream-for-n-steps funny-number-stream 20)
(stream-for-n-steps twoz 10)
(stream-for-n-steps dan-then-dog 5)
((cdr (stream-add-zero twoz)))
(car (stream-add-zero twoz))
(cdr (stream-add-zero twoz))
(stream-for-n-steps (lambda () (stream-add-zero funny-number-stream)) 15)
