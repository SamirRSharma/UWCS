;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname partition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 08, Problem 3
;; ***************************************************
;; got an extension, talk to Karen Anderson


;; (partition pred list) Produces a list of 2 lists, one that matches pred and ones that dont
;; Examples:
(check-expect (partition number? (list 1 "2" 3 "4" 5 "6")) (list (list 1 3 5) (list "2" "4" "6")))


;; partition: (Any -> Bool) (listof Any) -> (list (listof Any) (listof Any))
(define (partition pred list)
  (local [(define (s list) (cond [(empty? list) empty]
                                [(pred (first list)) (cons (first list) (s (rest list)))]
                                [else (s (rest list))]))
          (define (f list) (cond [(empty? list) empty]
                                [(pred (first list)) (f (rest list))]
                                [else (cons (first list) (f (rest list)))]))]
          (list (s list) (f list)))) 


;; Tests:

(check-expect (partition number? empty) (list empty empty))
(check-expect (partition string? (list "Math" 'Math 1)) (list (list "Math") (list 'Math 1)))


