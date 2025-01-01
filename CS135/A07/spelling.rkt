;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname spelling) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;    Samir Sharma (21116578)
;;    CS 135 Fall 2024
;;    A07 , Question 3
;; ***************************************************
;;

;;
;; Part a
;;

(define-struct node (char term? next))
;; A Node is a (make-node Char Bool Next)
;; Requires: Char is an uppercase letter (A-Z) or #\space 

;; A Next is one of:
;; - empty
;; - (cons Node Next)
;; Requires:  Nodes in Next are sorted in alphabetically ascending order of Char (A-Z)

;; node-template: Node -> Any
(define (node-template n)
  (... (node-char n)
       (node-term? n)
       (next-template (node-next n)) ...))

;; next-template: Next -> Any
(define (next-template nlist)
  (cond
    [(empty? nlist) ...]
    [else (... (node-template (first nlist))
               (next-template (rest nlist)))]))

;;
;; Part b
;;

;; (create-tree words) produces a Node that acts as the root node for a tree
;; Examples:
(check-expect (create-tree (list "FEW" "FUN"))
  (make-node #\space false (list
    (make-node #\F false (list
      (make-node #\E false (list
        (make-node #\W true empty)))
      (make-node #\U false (list
        (make-node #\N true empty))))))))

;; create-tree: (listof Str) -> Node
(define (create-tree words)
  (create-tree-add words (make-node #\space false empty)))

;; tests
(check-expect (create-tree (list "FUN" "FAR"))
  (make-node #\space false (list
    (make-node #\F false (list
      (make-node #\A false (list
        (make-node #\R true empty)))
      (make-node #\U false (list
        (make-node #\N true empty))))))))

(check-expect (create-tree (list "FUN" "fun" "fUn" "F"))
  (make-node #\space false (list
    (make-node #\F true (list
      (make-node #\U false (list
        (make-node #\N true empty))))))))

;; (create-tree-add words tree)  returns a new tree with the words added to tree
;; example
(check-expect
 (create-tree-add (list "FUN") (make-node #\space false empty))
 (make-node #\space false (list
   (make-node #\F false (list
     (make-node #\U false (list
       (make-node #\N true empty))))))))
;; create-tree-add: (listof Str) Node -> Node
(define (create-tree-add words tree)
  (cond
    [(empty? words) tree]
    [else (create-tree-add (rest words)
            (add-word tree (string->list (string-upcase (first words)))))]))

;; (add-word tree word-chars) produces added node
;; example
(define node-test (make-node #\F false empty))
(check-expect
 (add-word node-test (list #\U #\N))
 (make-node #\F false (list
   (make-node #\U false (list
     (make-node #\N true empty))))))
;; add-word: Node (listof Char) -> Node
(define (add-word tree word-chars)
  (cond
    [(empty? word-chars) (make-node (node-char tree) true (node-next tree))]
    [else
     (add-letter tree (first word-chars) (rest word-chars))]))

;; (add-letter tree letter rest-chars)  returns a new tree with the letter and rest-chars added
;; example
(define tree-test (make-node #\F false empty))
(check-expect
 (add-letter tree-test #\U (list #\N))
 (make-node #\F false (list
   (make-node #\U false (list
     (make-node #\N true empty))))))
;; add-letter: Node Char (listof Char) -> Node
(define (add-letter tree letter rest-chars)
  (cond
    [(false? (find-child letter (node-next tree)))
     (make-node (node-char tree) (node-term? tree)
                (insert-sorted
                  (create-subtree letter rest-chars)
                  (node-next tree)))]
    [else
     (make-node (node-char tree) (node-term? tree)
                (insert-sorted
                  (add-word (find-child letter (node-next tree)) rest-chars)
                  (remove-node letter (node-next tree))))]))

;; (find-child letter next) produces the child Node with char equal to letter
;; examples
(check-expect (find-child #\F (list (make-node #\F false empty))) (make-node #\F false empty))
(check-expect (find-child #\E (list (make-node #\F false empty))) false)
;; find-child: Char Next -> (oneof Node Bool)
(define (find-child letter next)
  (cond
    [(empty? next) false]
    [(char=? letter (node-char (first next))) (first next)]
    [else (find-child letter (rest next))]))

;; (insert-sorted node next) inserts node into next in sorted order
;; example
(check-expect (insert-sorted (make-node #\A false empty) empty)
              (list (make-node #\A false empty)))
(check-expect (insert-sorted (make-node #\B false empty) (list (make-node #\A false empty) (make-node #\C false empty)))
              (list (make-node #\A false empty) (make-node #\B false empty) (make-node #\C false empty)))
;; insert-sorted: Node Next -> Next
(define (insert-sorted node next)
  (cond
    [(empty? next) (list node)]
    [(char<? (node-char node) (node-char (first next)))
     (cons node next)]
    [else (cons (first next) (insert-sorted node (rest next)))]))

;; (remove-node letter next) removes the Node with char equal to letter from next
;; example
(check-expect (remove-node #\B (list (make-node #\A false empty) (make-node #\B false empty)))
              (list (make-node #\A false empty)))
(check-expect (remove-node #\C (list (make-node #\A false empty) (make-node #\B false empty)))
              (list (make-node #\A false empty) (make-node #\B false empty)))
;; remove-node: Char Next -> Next
(define (remove-node letter next)
  (cond
    [(empty? next) empty]
    [(char=? letter (node-char (first next))) (rest next)]
    [else (cons (first next) (remove-node letter (rest next)))]))

;; (create-subtree letter rest-chars) creates a subtree starting with letter and rest-chars
;; example
(check-expect (create-subtree #\A empty) (make-node #\A true empty))
(check-expect (create-subtree #\A (list #\B #\C)) 
              (make-node #\A false (list 
                (make-node #\B false (list 
                  (make-node #\C true empty))))))
;; create-subtree: Char (listof Char) -> Node
(define (create-subtree letter rest-chars)
  (cond
    [(empty? rest-chars)
     (make-node letter true empty)]
    [else
     (make-node letter false (list (create-subtree (first rest-chars) (rest rest-chars))))]))

;;
;; Part c
;;

;; (check word tree) produces true if the word is stored in the word-tree
;; examples
(define dict (create-tree (list "FEW" "FUN")))
(check-expect (check "few" dict) true)
(check-expect (check "FU" dict) false)
;; check: Str Node -> Bool
(define (check word tree)
  (check-char (string->list (string-upcase word)) tree))

;; tests
(check-expect (check "FUN" dict) true)
(check-expect (check "F" dict) false)

;; (check-char chars node) produces true if the word represented by chars is stored in the tree
;; examples
(check-expect (check-char (list #\F #\E #\W) (find-child #\F (node-next dict))) false)
(check-expect (check-char (list #\F #\U) (find-child #\F (node-next dict))) false)
;; check-char: (listof Char) Node -> Bool
(define (check-char chars node)
  (cond
    [(empty? chars) (node-term? node)]
    [else
     (find-child-and-check (first chars) (node-next node) (rest chars))]))

;; (find-child-and-check ch next rest-chars) searches for a child node with char 'ch' in 'next'
;; examples
(define test-next (list (make-node #\E false (list (make-node #\W true empty))) (make-node #\U false empty)))
(check-expect (find-child-and-check #\E test-next (list #\W)) 
              (check-char (list #\W) (make-node #\E false (list (make-node #\W true empty)))))
;; find-child-and-check: Char Next (listof Char) -> Bool
(define (find-child-and-check ch next rest-chars)
  (cond
    [(empty? next) false]
    [(char=? ch (node-char (first next)))
     (check-char rest-chars (first next))]
    [else
     (find-child-and-check ch (rest next) rest-chars)]))
