;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname funabst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Samir Sharma
;; CS 135 Fall 2024
;; Assignment 08, Problem 2
;; ***************************************************
;;

;; was given an extention, talk to Karen Anderson

;; Problem a

;; (or-pred pred list) produces true if if the predicate is in the list
;; Examples:

(check-expect (or-pred number? (list 2 3)) true)
(check-expect (or-pred string? empty) false)

;; or-pred: (X -> Bool) (listof X) -> Bool
(define (or-pred pred list)
  (cond [(empty? list) false]
        [(pred  (first list)) true]
        [else (or-pred pred (rest list))]))

;; Tests:

(check-expect (or-pred odd? (list 2 4 5 "yay")) true)
(check-expect (or-pred cons? (list (list 1) 1)) true)
(check-expect (or-pred number? (list (list 3) empty)) false)

;; Problem b

;; (map2argfn funlist list) produces list of applied functions to second list
;; Examples:

(check-expect (map2argfn (list + - / *) (list 1 1)) (list 2 0 1 1))

;; map2argfn: (listof (Num Num -> Any)) (list Num Num) -> (listof Any)
(define (map2argfn funlist list)
  (cond [(empty? funlist) empty]
        [else
         (cons ((first funlist) (first list) (second list))
                    (map2argfn (rest funlist) list))]))

;; Tests:

(check-expect (map2argfn empty (list 2 3)) empty)
(check-expect (map2argfn (list - + -) (list 1 1)) (list 0 2 0))
(check-expect (map2argfn (list +) (list 1 2)) (list 3))


;; Problem c

;; (arranged? checklist checkinglist) produces true if pred function true, and ordered properly
;; Examples:

(check-expect (arranged? (list string? <) (list "Test" 3)) false)

;; arranged?: (list (Any -> Bool) (X X -> Bool)) (listof Any) -> Bool
;; requires: if binary-relational-operator is applied on any
;; elements, then predicate-function produces true on
;; elements of type X
(define (arranged? checklist checkinglist)
  (cond [(empty? checkinglist) true]
        [(empty? (rest checkinglist))
         ((first checklist) (first checkinglist))]
        [(and (and ((first checklist) (first checkinglist)) ((first checklist) (second checkinglist)))
             ((second checklist) (first checkinglist) (second checkinglist)))
         (arranged? checklist (rest checkinglist))]
        [else false]))

;; Tests:

(check-expect (arranged? (list number? >) (list 5 4 3 2 1)) true)
(check-expect (arranged? (list number? >) (list 2)) true)