;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname morelistfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 6, Problem 1
;; ***************************************************
;;

;; got extention for this assignment until friday 9pm est

;;
;; Part a
;;

;; (my-list-ref lon idx) produces the element at the index

;; Examples:
(check-expect (my-list-ref (list 1 2 3 4) 0) 1)


;; my-list-ref: (listof Num) Nat -> (anyof Num false)
(define (my-list-ref lon index)
  (cond
    [(empty? lon) false]
    [(zero? index) (first lon)]
    [else (my-list-ref (rest lon) (- index 1))]))

;; Tests:
(check-expect (my-list-ref (list 5 4 3) 2) 3)
(check-expect (my-list-ref (list 2) 20) false)

;;
;; Part b
;;

;; (zip list1 list2) produces an association pairing list
;; Examples:
(check-expect (zip (list 1 2 3 4) (list "a" "b" "c" "d"))
              (list (list 1 "a") (list 2 "b") (list 3 "c") (list 4 "d")))

;; zip: (listof Any) (listof Any) -> (listof (list Any Any))
;; Requires: (length list1) = (length list2)
(define (zip list1 list2)
  (cond
    [(empty? list1) empty]
    [else (cons (list (first list1) (first list2))
                (zip (rest list1) (rest list2)))]))

;; Tests:
(check-expect (zip (list 1 2 3 4) (list "a" "b" "c" "d"))
              (list (list 1 "a") (list 2 "b") (list 3 "c") (list 4 "d")))

(check-expect (zip (list 1) (list 2)) (list (list 1 2)))

;;
;; Part c
;;


;; (list-xor list1 list2) produces a sorted list of items

;; Examples:

(check-expect (list-xor (list 1 3 5) (list 2 3 4)) (list 1 2 4 5))

;; list-xor: (listof Num) (listof Num) -> (listof Num)
;; Requires: list1 and list2 are sorted 
(define (list-xor list1 list2)
  (cond
    [(empty? list1) list2]
    [(empty? list2) list1]
    [(< (first list1) (first list2))
        (cons (first list1) (list-xor (rest list1) list2))]
    [(> (first list1) (first list2))
        (cons (first list2) (list-xor list1 (rest list2)))]
    [else
        (list-xor (rest list1) (rest list2))]))


;; Tests:
(check-expect (list-xor empty (list 1)) (list 1))

(check-expect (list-xor empty empty) empty)

(check-expect (list-xor (list 1 3) (list 1 3)) empty)
