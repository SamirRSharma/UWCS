;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 01, Problem 4
;; ***************************************************
;;

;; Question a

(define ( m/s->mph x) (* (/ 3600 1609.344) x ))

(check-expect ( m/s->mph 1609.344) 3600)

;; Question b

(define (mph->s/mfn y) (* y (/ (* 1609.344 1209.6) (* 1.7018 3600 ))))
(check-expect (mph->s/mfn 1) (+ 317 (/ 1249 1675)))