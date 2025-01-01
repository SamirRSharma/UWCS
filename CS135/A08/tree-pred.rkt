;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tree-pred) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 08, Problem 3
;; ***************************************************
;; got an extension, talk to Karen Anderson

(define-struct node (key left right))
;; A Node is a (make-node Nat BT BT)
;; A Binary Tree (BT) is one of:
;; * empty
;; * Node

;; (tree-pred pred) Produces true if all nodes in tree match predicate
;; Examples:

(check-expect ((tree-pred number?) (make-node 1 2 3)) true)


;; tree-pred: (Nat -> Bool) -> (BT -> Bool)
(define (tree-pred pred)
  (local [(define (f tree) (cond
                             [(empty? tree) true]
                             [(pred (node-key tree))
                              (and (f (node-left tree)) (f (node-right tree)))]
                             [else false]))]
      f))

;; Tests:

(check-expect ((tree-pred negative?) empty) true)
(check-expect ((tree-pred odd?) (make-node 4 empty (make-node 1 empty empty))) false)
(check-expect ((tree-pred even?) (make-node 2 (make-node 3 empty empty) empty)) true)
(check-expect ((tree-pred odd?) (make-node 1 empty empty)) false)


