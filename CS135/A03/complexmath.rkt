;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname complexmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define-struct point (x y))
;; A Point is a (make-point Num Num)

;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 03, Problem 1
;; ***************************************************
;;


;; (point-mult p1 p2) produces (x1,y1) and (x2,y2) multiplied together

;; Example usage:
(check-expect (point-mult (make-point 1 2) (make-point 3 4)) (make-point -5 10))
(check-expect (point-mult (make-point 6 5) (make-point 7 8)) (make-point 2 83))


;; point-mult: Point Point -> Point
(define (point-mult p1 p2)
  (make-point
   (- (* (point-x p1) (point-x p2)) (* (point-y p1) (point-y p2)))
   (+ (* (point-x p1) (point-y p2)) (* (point-x p2) (point-y p1)))))

;; tests

(check-expect (point-mult (make-point 2 11) (make-point 7 1)) (make-point 3 79))
(check-expect (point-mult (make-point 2 2) (make-point 2 2)) (make-point 0 8))





;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 03, Problem 1b
;; ***************************************************
;;

;; Example usage:
(check-expect (point-div (make-point 4 2) (make-point 1 1)) (make-point 3 -1))
(check-expect (point-div (make-point 8 4) (make-point 2 2)) (make-point 3 -1))

;; (point-div p3 p4) produces (x1, y1) divided by (x2, y2)
;; Requires: (point-x p4)^2 + (point-y p4)^2 cant equal 0 
;; point-div: Point Point -> Point
(define (point-div p1 p2)
  (make-point
   (/
    (+ (* (point-x p1) (point-x p2)) (* (point-y p1) (point-y p2)))
    (+ (sqr (point-x p2)) (sqr (point-y p2))))
   (/
    (- (* (point-y p1) (point-x p2)) (* (point-x p1) (point-y p2)))
    (+ (sqr (point-x p2)) (sqr (point-y p2))))))

;; tests




(check-expect (point-div (make-point 6 0) (make-point 3 0)) (make-point 2 0))
(check-expect (point-div (make-point 9 3) (make-point 3 3)) (make-point 2 -1))
(check-expect (point-div (make-point 12 6) (make-point 3 3)) (make-point 3 -1))


