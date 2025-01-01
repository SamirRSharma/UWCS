;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname debug-a01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Samir Sharma (DO 21116578)
;; CS 135 Fall 2024
;; Assignment 01, Problem 1
;; ***************************************************
;;

  (define (luminosity red-value green-value blue-value)
    ( +  ( * 0.3 red-value) ( * 0.59 green-value) ( * 0.11 blue-value) ) )


( check-expect (luminosity 1 1 1) 1 )

( check-expect (luminosity 2 2 2) 2)
