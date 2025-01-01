;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 01, Problem 2
;; ***************************************************
;;

;; Question a

( define (manhattan-distance x1 y1 x2 y2)
   (+ (abs (- x1 x2)) (abs (- y1 y2))
      )
        )

;; Question b

( define (batter-slugging-average s d t hr ab)
   ( / (+ s (* 2 d) (* 3 t) (* 4 hr)) ab ))

;; Question c


(define (cone-area r h)
  (* (* pi r) (+ r (sqrt (+ (sqr h) (sqr r)) ))  ))

;; Question d

(define G 6.674e-11 )

(define (escape M r) (sqrt (/ (* 2 G M) r)))

;; Question e

(define (partition-size-approximation n)
  (* (/ 1 (* 4 n (sqrt 3)))
     (exp (* pi (sqrt (/ (* 2 n) 3))))
     ))

;; Question f

(define (d1 maturity rate volatility spot-price strike-price)
  (* (/ 1 (* volatility (sqrt maturity)))
     (+ (log (/ spot-price strike-price)) (* (+ rate (/ (sqr volatility) 2)) maturity))))