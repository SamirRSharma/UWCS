;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname symbol-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; [Samir Sharma] (21116578)
;; CS 135 Fall 2024
;; Assignment 05, Problem 4
;; ***************************************************
;;


;; (make-symbol-list natlist symbol) produces nested list

;; example
(check-expect (make-symbol-lists (list 1 1 2) 'A)
              (list (list 'A) (list 'A) (list 'A 'A)))

;; (make-symbol-list natlist symbol) (listof Nat) Sym -> (listof (listof Sym))
(define (make-symbol-lists natlist symbol)
  (cond
    [(empty? natlist) empty]
    [else (cons (inner-list 0 (first natlist) symbol) (make-symbol-lists (rest natlist) symbol))]
   ))

;; tests

(check-expect
 (make-symbol-lists (list 3 5 4) 'Y)
 (list (list 'Y 'Y 'Y) (list 'Y 'Y 'Y 'Y 'Y) (list 'Y 'Y 'Y 'Y)))

(check-expect
 (make-symbol-lists empty 'Z)
 empty)

(check-expect
 (make-symbol-lists (list 1 1 1) 'A)
 (list (list 'A) (list 'A) (list 'A)))

;; (inner-list start end sym) produces the inner list recursively

;; example

(check-expect (inner-list 0 3 'B) (list 'B 'B 'B))

;; (inner-list start end sym): Int Nat Sym -> (listof Sym)
(define (inner-list start end sym)
  (cond 
    [(>= start end) empty]
    [else (cons sym (inner-list (add1 start) end sym))]
  ))


