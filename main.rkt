#lang racket

; P01
(define (my-last input) 
  (if (null? input) 
    null
    (if (null? (cdr input))
      (car input)
      (my-last (cdr input)))))

; P02
(define (my-but-last input)
  (case (length input)
    [(0 1) null]
    [(2) input]
    [else (my-but-last (cdr input))]))

; P03
(define (element-at input k)
  (letrec ([at-inner 
             (lambda (input k accum) 
                 (if (null? input)
                   null
                   (if (equal? k accum)
                     (car input)
                     (at-inner (cdr input) k (+ accum 1)))))])
    (at-inner input k 1)))

; P04
(define (my-count input) 
  (letrec ([count-inner
          (lambda (input n)
            (if (null? input)
              n
              (count-inner (cdr input) (+ n 1))))])
    (count-inner input 0)))
