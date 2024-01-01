;;; My implementation of "L-99: Ninety-Nine Lisp Problems"
#lang typed/racket

;; P01
;; 
;; Returns the last element of a list.
;; Example:
;; > (my-last '(1 2 3))
;; 3
(: my-last (-> (Listof Number) (U Number Null))) 
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
(: my-but-last (-> (Listof Number) (U (Listof Number) Null)))
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
(: element-at (-> (Listof Number) Number (U Number Null)))
(define (element-at input k)
  (letrec ([at-inner : (-> (Listof Number) Number Number (U Number Null)) 
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
(: my-count (-> (Listof Number) Number))
(define (my-count input) 
  (letrec ([count-inner : (-> (Listof Number) Number Number)
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
(: rev (-> (Listof Number) (Listof Number)))
(define (rev input) 
  (letrec ([rev-inner : (-> (Listof Number) (Listof Number) (Listof Number))
             (lambda (input accum)
               (if (null? input)
                 accum
                 (rev-inner (cdr input) (cons (car input) accum))))])
    (rev-inner input null)))

;; P06
;;
;; Example:
;; > (palindrome '(3 2 1 2 3))
;; #t
(: palindrome (-> (Listof Number) Boolean))
(define (palindrome input)
  (equal? input (rev input)))

;; P07
;;
;; Example:
;; > (my-flatten '((1 2) 3))
;; '(1 2 3)
(: my-flatten (-> (Listof (U (Listof Number) Number)) (Listof Number)))
(define (my-flatten input)
  (cond 
    [(null? input) '()]
    [(list? (car input)) (append (my-flatten (car input)) (my-flatten (cdr input)))]
    [else (cons (car input) (my-flatten (cdr input)))]))

;; P08
;;
;; Example:
;; > (compress '(a a a b b c c c c))
;; '(a b c)
(: compress (-> (Listof Symbol) (Listof Symbol)))
(define (compress input)
  (letrec ([compress-inner : (->  (Listof Symbol) (U Symbol Null) (Listof Symbol) (Listof Symbol)) 
             (lambda (input prev accum)
                             (cond 
                               [(null? input) accum]
                               [(equal? prev (car input)) (compress-inner (cdr input) prev accum)]
                               [else (compress-inner (cdr input) (car input) (append accum (list (car input))))]))])
    (compress-inner input null '())))

;; P09
;;
;; Example:
;; > (pack '(a a a b b c c c c))
;; '((a a a) (b b) (c c c c))
(: pack (-> (Listof Symbol) (Listof (Listof Symbol))))
(define (pack input) 
  (letrec ([pack-inner : (-> (Listof Symbol) (Listof Symbol) (Listof (Listof Symbol)) (Listof (Listof Symbol))) 
             (lambda (input prev accum)
                         (cond
                           [(null? input) (append accum (list prev))]
                           [(null? prev) (pack-inner (cdr input) (list (car input)) accum)]
                           [(equal? (car prev) (car input)) (pack-inner (cdr input) (append prev (list (car input))) accum)]
                           [else (pack-inner (cdr input) (list (car input)) (append accum (list prev)))]))])
    (if (null? input) 
      '() 
      (pack-inner input '() '()))))
