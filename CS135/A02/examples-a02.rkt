;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname examples-a02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 02
;; ***************************************************
;;


;; Question 2 check expect

(define (median-of-3-simple a b c) 1)

(check-expect (median-of-3-simple 3 2 1) 2)
(check-expect (median-of-3-simple 5 3 1) 3)
(check-expect (median-of-3-simple 1 3 5) 3)
(check-expect (median-of-3-simple 1 100 1000) 100)
;; 4a

(define (can-donate-to/cond? x y) x)
(check-expect (can-donate-to/cond? 'O- 'A+) true)
(check-expect (can-donate-to/cond? 'O+ '0+) false)
(check-expect (can-donate-to/cond? 'O- '0+) true)
(check-expect (can-donate-to/cond? 'AB+ 'AB+) true)
(check-expect (can-donate-to/cond? 'AB- 'B-) false)


;; #5

(define (check-plan a b c) 1)
(check-expect (check-plan 'Charlottetown 'QuebecCity 'Vancouver 'Vancouver) 'invalid)

(check-expect (check-plan 'Calgary 'Calgary 'Calgary 'Calgary) 'rested)  ; CALGARY WHOOOOOOOO


(check-expect (check-plan 'SaultSteMarie 'ThunderBay 'Winnipeg 'Regina) 'ready)
