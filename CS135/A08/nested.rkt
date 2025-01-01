;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nested) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 08, Problem 4
;; ***************************************************
;; got an extension, talk to Karen Anderson

;; A (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))
;; Requires: X itself is not a list type

;; Problem a

;; nested-listof-X-template: (nested-listof X) -> Any
(define (nested-listof-X-template nlox)
  (cond [(empty? nlox) ...]
        [(cons? (first nlox))
         (... (nested-listof-X-template (first nlox))  (nested-listof-X-template (rest nlox)))]
        [(cons? nlox)
         (... (X-template (first nlox)) (nested-listof-X-template (rest nlox)))]))

;; Problem b

;; (nested-filter pred nlist) Produces a nested-listof X with only elements that match the predicate
;; Examples:

(check-expect (nested-filter number? (list 1 (list "Two") 3 4)) (list 1 empty 2 4))

;; nested-filter: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (nested-filter pred nlist)
  (cond [(empty? nlist) empty]
        [(cons? (first nlist))
         (cons (nested-filter pred (first nlist)) (nested-filter pred (rest nlist)))]
        [(pred (first nlist))
         (cons (first nlist) (nested-filter pred (rest nlist)))]
        [else (nested-filter pred (rest nlist))]))

;; tests

(check-expect (nested-filter boolean? empty) empty)
(check-expect (nested-filter number? (list "RealNumberTrust")) empty)

;; Problem c

;; (ruthless symlist) Produces a list without 'ruth
;; Examples:
(check-expect (ruthless '(ruth)) empty)

;; ruthless: (nested-listof Sym) -> (nested-listof Sym)
(define (ruthless symlist)
  (local [(define (not-ruth? sym) (not (symbol=? 'ruth sym)))]
    (nested-filter not-ruth? symlist)))

;; Tests:
(check-expect (ruthless empty) empty)
(check-expect (ruthless '(NotRuth)) '(NotRuth))
(check-expect (ruthless '((ruth))) '(()))

;; Problem d

;; (keep-between a b numlist) produces a list of numbers between a and b
;; Examples:

(check-expect (keep-between 2 4 (list 1 3 5)) (list 3))


;; keep-between: Num Num (nested-listof Num) -> (nested-listof Num)
(define (keep-between a b numlist)
  (local [(define (between? a b)
            (local [(define (f c)
                      (cond [(or (and (>= c a) (<= c b)) (and (>= c b) (<= c a)))
                             true]
                            [else false]))]
               f))]
    (nested-filter (between? a b) numlist)))

;; Tests:

(check-expect (keep-between 4 4 empty) empty)
(check-expect (keep-between 99 100 (list 1 2 3)) empty)


;; Problem e

;; (nested-cleanup listemptys) produces a nested list with all empty lists removed
;; Examples:

(check-expect (nested-cleanup '(3 (()()) 2 ((1 () (()))) )) '(3 2 ((2))))

;; nested-cleanup: (nested-listof Any) -> (anyof (nested-listof Any) false)

(define (nested-cleanup listemptys)
  (local [(define f (cond [(empty? listemptys) false]
                          [(boolean? (nested-cleanup (rest listemptys))) empty]
                          [else (nested-cleanup (rest listemptys))]))]
   (cond [(empty? listemptys) false]
          [(and
            (cons? (first listemptys)) (empty? (nested-cleanup (first listemptys)))) f]
          [(cons? (first listemptys))
           (cons (nested-cleanup (first listemptys)) f)]
          [(empty? (first listemptys)) f]
          [else (cons (first listemptys) f)])))

;; Tests:

(check-expect (nested-cleanup empty) false)
(check-expect (nested-cleanup (list 1 (list (list empty) empty))) (list 1))

;; Problem f

;; (nested-apply funlist numlist) Produces a list with the functions in funlist applied to it
;; Examples:

(check-expect (nested-apply (list sqr) '(1 2 3)) (list '(1 4 9)))

;; nested-apply: (listof (anyof (Num -> Num) (Num -> Int) (Num -> Nat)))
;;               (nested-listof Num) -> (listof (nested-listof Num))

(define (nested-apply funlist numlist)
  (local [(define (nested-apply-one operator numlist)
            (cond [(empty? numlist) empty]
                  [(empty? (first numlist))
                   (cons empty (nested-apply-one operator (rest numlist)))]
                  [(cons? (first numlist))
                   (cons (nested-apply-one operator (first numlist)) (nested-apply-one operator (rest numlist)))]
                  [else (cons (operator (first numlist)) (nested-apply-one operator (rest numlist)))]))]
     (cond [(empty? funlist) empty]
           [else (cons (nested-apply-one (first funlist) numlist) (nested-apply (rest funlist) numlist))])))

;; Tests:

(check-expect (nested-apply (list abs) '(-1)) (list '(1)))
(check-expect (nested-apply (list sqr floor) empty) (list empty empty))