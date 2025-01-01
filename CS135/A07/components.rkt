;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname components) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;    Samir Sharma (21116578)
;;    CS 135 Fall 2024
;;    A07 , Question 1
;; ***************************************************
;;

;; was given an extention until friday 9pm, talk to karen anderson

(define-struct component (name num subcomponents))
;; A Component is a (make-component Str Nat (listof Component))
;; Requires: num must be larger than 0



;; (contains-component? component name) produces true if the string is in the component
;; example
(define bike (make-component "bike" 1 (list
                                       (make-component "frame" 1 empty)
                                       (make-component "wheel" 2 (list
                                                                  (make-component "tire" 1 empty)
                                                                  (make-component "rim" 1 empty)
                                                                  (make-component "spoke" 30 empty)
                                                                  (make-component "hub" 1 (list
                                                                                           (make-component "housing" 1 empty)
                                                                                           (make-component "axel" 1 empty)
                                                                                           (make-component "bearing" 20 empty)))))
                                       (make-component "seat" 1 empty)
                                       (make-component "handlebar" 1 empty))))
(check-expect (contains-component? bike "handlebar") true)

;; (contains-component? component name): Component Str -> Bool
(define (contains-component? component name)
  (or
    (string=? (component-name component) name)
    (contains-in-list (component-subcomponents component) name)))


;; tests
(check-expect (contains-component? bike "frame") true)
(check-expect (contains-component? bike "horn") false)
(check-expect (contains-component? bike "chain") false)


;; (contains-in-list component-list name) produces true if complist contains the nam
;; Example:
(check-expect (contains-in-list (list (make-component "handle" 2 empty)) "handle") true)

;; contains-in-list: (listof Component) Str -> Bool
(define (contains-in-list complist name)
  (cond
    [(empty? complist) false]
    [else (or (contains-component? (first complist) name)
              (contains-in-list (rest complist) name))]))