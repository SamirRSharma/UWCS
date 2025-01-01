;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 6, Problem 2
;; ***************************************************
;;

;; got extention for this assignment until friday 9pm est


;; global helper



;; (my-list-ref lon idx) produces the element at the index

;; Examples:
(check-expect (my-list-ref (list 1 2 3 4) 0) 1)

;; my-list-ref: (listof Num) Nat -> (anyof Num false)
(define (my-list-ref lon index)
  (cond
    [(empty? lon) false]
    [(zero? index) (first lon)]
    [else (my-list-ref (rest lon) (- index 1))]))



;;
;; Part a
;;


;; A Matrix is a (listof (listof Num))
;; Requires: All rows have the same length

;;
;; Part b
;;

;; (matrix-item mat row col) produces the item at position
;; Examples:
(check-expect (matrix-item (list (list 1 2 3 4) (list 5 6 7 8)) 0 0) 1)

;; matrix-item: Matrix Nat Nat -> Num
(define (matrix-item mat row col)
  (my-list-ref (my-list-ref mat row) col))

;; Tests:
(check-expect (matrix-item (list (list 1 2 3 4) (list 5 6 7 8)) 1 3) 8)

(check-expect (matrix-item (list (list 1 2 3 4) (list 5 6 7 8)) 0 1) 2)


;;
;; Part c
;;

;; (matrix-col mat col) produces the specified column from mat.
;; Examples:
(check-expect (matrix-col (list (list 1 2 3 4) (list 5 6 7 8)) 2) (list 3 7))

;; matrix-col: Matrix Nat -> (listof Num)
(define (matrix-col mat col)
  (cond
    [(empty? mat) empty]
    [else (cons (my-list-ref (first mat) col)
                (matrix-col (rest mat) col))]))

;; Tests:
(check-expect (matrix-col (list (list 1 2 3 4) (list 5 6 7 8)) 0) (list 1 5))

(check-expect (matrix-col (list (list 1 2 3 4) (list 5 6 7 8)) 3) (list 4 8))

;;
;; Part d
;;


;; (matrix-transpose mat) produces the transpose of matrix
;; Examples:
(check-expect (matrix-transpose (list (list 1 2 3 4) (list 5 6 7 8)))
              (list (list 1 5) (list 2 6) (list 3 7) (list 4 8)))

;; matrix-transpose: Matrix -> Matrix
(define (matrix-transpose mat)
  (cond
    [(empty? (first mat)) empty]
    [else (cons (get-first-column mat)
                (matrix-transpose (get-rest-columns mat)))]))

;; Tests:
(check-expect (matrix-transpose (list (list 1 2) (list 3 4))) (list (list 1 3) (list 2 4)))

(check-expect (matrix-transpose (list (list 1) (list 3))) (list (list 1 3)))



;; (get-first-column mat) produces the first column of the matrix
;; Example:
(check-expect (get-first-column (list (list 1 2) (list 3 4))) (list 1 3))

;; get-first-column: Matrix -> (listof Any)
(define (get-first-column mat)
  (cond
    [(empty? mat) empty]
    [else (cons (first (first mat))
                (get-first-column (rest mat)))]))


;; (get-rest-columns mat) produces mat without the first column
;; Example:
(check-expect (get-rest-columns (list (list 1 2) (list 3 4))) (list (list 2) (list 4)))

;; get-rest-columns: Matrix -> Matrix
(define (get-rest-columns mat)
  (cond
    [(empty? mat) empty]
    [else (cons (rest (first mat))
                (get-rest-columns (rest mat)))]))

