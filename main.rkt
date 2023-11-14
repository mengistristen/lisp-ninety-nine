;;; My implementation of "L-99: Ninety-Nine Lisp Problems"
#lang racket

;; P01
;; 
;; Returns the last element of a list.
;; Example:
;; > (my-last '(1 2 3))
;; 3
(define (my-last input) 
  (if (null? input) 
    null
    (if (null? (cdr input))
      (car input)
      (my-last (cdr input)))))

;; P02
;;
;; Returns the last two element of a list.
;; Example:
;; > (my-but-last '(1 2 3))
;; '(2 3)
(define (my-but-last input)
  (case (length input)
    [(0 1) null]
    [(2) input]
    [else (my-but-last (cdr input))]))

;; P03
;;
;; Returns the element at the k-th position in a list.
;; Example:
;; > (element-at '(1 2 3) 2)
;; 2
(define (element-at input k)
  (letrec ([at-inner 
             (lambda (input k accum) 
                 (if (null? input)
                   null
                   (if (equal? k accum)
                     (car input)
                     (at-inner (cdr input) k (+ accum 1)))))])
    (at-inner input k 1)))

;; P04
;;
;; Returns the number of elements in a list.
;; Example:
;; > (my-count '(1 2 3))
;; 3
(define (my-count input) 
  (letrec ([count-inner
          (lambda (input n)
            (if (null? input)
              n
              (count-inner (cdr input) (+ n 1))))])
    (count-inner input 0)))

;; P05
;;
;; Reverses the input list.
;; Example:
;; > (rev '(1 2 3))
;; '(3 2 1)
(define (rev input) 
  (letrec ([rev-inner
          (lambda (input accum)
            (if (null? input)
               accum
              (rev-inner (cdr input) (cons (car input) accum))))])
    (rev-inner input null)))
