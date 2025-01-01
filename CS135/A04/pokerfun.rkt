;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pokerfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 04, Problem 3
;; ***************************************************
;;


;; WAS GIVEN a 3 DAY EXTENSION BY ACCESSABILITY SERVICES

(define-struct card (rank suit))
;; A Card is a (make-card Rank Suit)
;; where Rank is (anyof 2 3 4 5 6 7 8 9 10 'Jack 'Queen 'King 'Ace)
;; and Suit is one of 'Club, 'Diamond', 'Heart', 'Spade
;; Requires:
;; - rank is either an integer from 2 to 10 or one of 'Jack, 'Queen, 'King, 'Ace
;; - suit is one of 'Club, 'Diamond, 'Heart, 'Spade


;; Global Helpers for multiple functions  ------------------------------------------------


;; (rank-value rank) produces the numerical value of rank

;; Examples:
(check-expect (rank-value 2) 1)
(check-expect (rank-value 'Jack) 10)

;; rank-value: Rank -> Nat
;; rank-value: Rank -> Nat
(define (rank-value rank)
  (cond
    [(number? rank) (- rank 1)]
    [(symbol=? rank 'Jack) 10]
    [(symbol=? rank 'Queen) 11]
    [(symbol=? rank 'King) 12]
    [(symbol=? rank 'Ace) 13]))

;; (suit-value suit) produces the numerical value of suit
;; Examples:
(check-expect (suit-value 'Spade) 4)

;; suit-value: Suit -> Nat
(define (suit-value suit)
  (cond
    [(symbol=? suit 'Club) 1]
    [(symbol=? suit 'Diamond) 2]
    [(symbol=? suit 'Heart) 3]
    [(symbol=? suit 'Spade) 4]))


;; Problem 3(a)    ------------------------------------------------


;; (comparecard card1 card2) checks if card 1 is less then card 2
;; Examples:
(check-expect (comparecard (make-card 2 'Club) (make-card 2 'Diamond)) true)
(check-expect (comparecard (make-card 'Ace 'Spade) (make-card 'Ace 'Club)) false)

;; comparecard Card Card -> Bool
(define (comparecard card1 card2)
  (cond
    [(< (rank-value (card-rank card1))
        (rank-value (card-rank card2)))
     true]
    [(= (rank-value (card-rank card1))
        (rank-value (card-rank card2)))
     (< (suit-value (card-suit card1))
        (suit-value (card-suit card2)))]
    [else false]))

;; (sorted? loc) produces true if the Card are in increasing order

;; Examples:
(check-expect
 (sorted? (cons (make-card 3 'Diamond)
   (cons (make-card 8 'Club) empty)))
 true)

;; sorted?: (non empty listof Card) -> Bool
(define (sorted? loc)
  (cond
    [(empty? (rest loc)) true]
    [else
     (and (comparecard (first loc) (first (rest loc))) (sorted? (rest loc)))]))

;; Tests:
(check-expect
 (sorted? (cons (make-card 2 'Club)
   (cons (make-card 3 'Club)
     (cons (make-card 4 'Club)
       (cons (make-card 5 'Club)
         (cons (make-card 6 'Club) empty))))))
 true)
(check-expect
 (sorted? (cons (make-card 3 'Spade)
   (cons (make-card 2 'Spade) empty)))
 false)


;; Problem 3(b)     ------------------------------------------------


;; (cheater? loc) produces true if contains duplicate cards,


;; Examples:
(check-expect
 (cheater? (cons (make-card 3 'Diamond)
   (cons (make-card 4 'Spade)
     (cons (make-card 5 'Club) empty))))
 false)
(check-expect
 (cheater? (cons (make-card 4 'Spade)
   (cons (make-card 4 'Spade) empty)))
 true)

;; cheater?: (non empty listof Card) -> Bool
;; Requires: loc is a sorted list of 5 cards
(define (cheater? cards)
  (cond
    [(empty? (rest cards)) false]
    [(and (= (card-rank (first cards)) (card-rank (first (rest cards))))
          (symbol=? (card-suit (first cards)) (card-suit (first (rest cards)))))
     true]
    [else (cheater? (rest cards))]))


(check-expect
 (cheater? (cons (make-card 3 'Club)
   (cons (make-card 3 'Diamond)
     (cons (make-card 3 'Heart)
       (cons (make-card 3 'Spade) empty)))))
 false)

(check-expect
 (cheater? (cons (make-card 5 'Heart)
   (cons (make-card 5 'Heart)
     (cons (make-card 7 'Diamond) empty))))
 true)


;; Problem 3(c)     ------------------------------------------------

;; (is-straight? loc) produces true if forms a straight,

;; Examples:
(check-expect
 (is-straight? (cons (make-card 2 'Diamond)
   (cons (make-card 3 'Heart)
     (cons (make-card 4 'Club)
       (cons (make-card 5 'Spade)
         (cons (make-card 6 'Club) empty))))))
 true)

(check-expect
 (is-straight? (cons (make-card 8 'Diamond)
   (cons (make-card 9 'Club)
     (cons (make-card 9 'Heart)
       (cons (make-card 3 'Spade)
         (cons (make-card 3 'Club) empty))))))
 false)


;; is-straight?: (listof Card) -> Bool
;; Requires: loc is a sorted list of 5 cards
(define (is-straight? cards)
  (cond
    [(empty? (rest cards)) true]
    [(= (- (rank-value (card-rank (first (rest cards))))
           (rank-value (card-rank (first cards))))
        1)
     (is-straight? (rest cards))]
    [else false]))

;; Tests:
(check-expect
 (is-straight? (cons (make-card 2 'Diamond)
   (cons (make-card 3 'Heart)
     (cons (make-card 4 'Club)
       (cons (make-card 5 'Spade)
         (cons (make-card 6 'Club) empty))))))
 true)

(check-expect
 (is-straight? (cons (make-card 2 'Diamond)
   (cons (make-card 3 'Heart)
     (cons (make-card 4 'Club)
       (cons (make-card 5 'Spade)
         (cons (make-card 8 'Club) empty))))))
 false)


;; Problem 3(d)      ------------------------------------------------

;; (is-flush? loc) produces true if all cards in loc have the same suit
;; Examples:
(check-expect
 (is-flush? (cons (make-card 3 'Diamond)
   (cons (make-card 4 'Diamond)
     (cons (make-card 6 'Club)
       (cons (make-card 7 'Spade) 
         (cons (make-card 8 'Club) empty))))))
 false)

(check-expect
 (is-flush? (cons (make-card 2 'Club)
   (cons (make-card 3 'Club)
     (cons (make-card 5 'Club)
       (cons (make-card 6 'Club)
         (cons (make-card 7 'Club) empty))))))
 true)

;; is-flush?: (listof Card) -> Bool
;; Requires: loc is a sorted list of 5 cards
(define (is-flush? cards)
  (cond
    [(empty? (rest cards)) true]
    [(= (suit-value (card-suit (first cards))) (suit-value (card-suit (first (rest cards)))))
     (is-flush? (rest cards))]
    [else false]))


;; Tests:
(check-expect
 (is-flush? (cons (make-card 2 'Heart)
   (cons (make-card 5 'Heart)
     (cons (make-card 7 'Heart)
       (cons (make-card 8 'Heart)
         (cons (make-card 9 'Heart) empty))))))
 true)

(check-expect
 (is-flush? (cons (make-card 2 'Club)
   (cons (make-card 5 'Diamond)
     (cons (make-card 6 'Club)
       (cons (make-card 7 'Club)
         (cons (make-card 8 'Club) empty))))))
 false)

;; Problem 3(e)      ------------------------------------------------

;; Recursively checks if the first n cards have the same rank_value

;; examples


;; Define some example cards
(define card1 (make-card 3 'Hearts))
(define card2 (make-card 3 'Diamonds))
(define card3 (make-card 3 'Clubs))
(define card4 (make-card 5 'Spades))
(define card5 (make-card 6 'Hearts))

;; example
(check-expect
 (same-rank-first-n (list (make-card 3 'Hearts) (make-card 3 'Diamonds)
                          (make-card 3 'Clubs) (make-card 5 'Spades) (make-card 6 'Hearts))
                    3 (rank-value 3))
 true)

;; same-rank-first-n: Listof Card, Nat, Nat -> Boolean
(define (same-rank-first-n cards n rank-val)
  (cond
    [(= n 0) true]
    [(empty? cards) false]
    [else
     (and
       (= (rank-value (card-rank (first cards))) rank-val)
       (same-rank-first-n (rest cards) (- n 1) rank-val))]))

;; Returns the list after skipping the first n elements

;; example

(check-expect
 (rest-n (list (make-card 2 'Hearts)
               (make-card 3 'Clubs)
               (make-card 4 'Diamonds)
               (make-card 5 'Spades)
               (make-card 6 'Hearts))
         2)
 (list (make-card 4 'Diamonds)
       (make-card 5 'Spades)
       (make-card 6 'Hearts)))

;; rest-n: Listof Card, Nat -> Listof Card
(define (rest-n cards n)
  (cond
    [(= n 0) cards]
    [(empty? cards) empty]
    [else (rest-n (rest cards) (- n 1))]))






;; (is-full-house? hand) produces true if hand is a full house

;; Examples:
(check-expect
 (is-full-house? (list (make-card 2 'Diamond)
                       (make-card 3 'Heart)
                       (make-card 4 'Club)
                       (make-card 5 'Spade)
                       (make-card 6 'Club)))
 false)

(check-expect
 (is-full-house? (list (make-card 3 'Club)
                       (make-card 3 'Diamond)
                       (make-card 5 'Club)
                       (make-card 5 'Heart)
                       (make-card 5 'Spade)))
 true)

;; is-full-house?: (list Card Card Card Card Card) -> Bool
;; Requires: hand is a sorted list of 5 Cards
(define (is-full-house? cards)
  (cond
    [(and
      (same-rank-first-n cards 3 (rank-value (card-rank (first cards))))
      (same-rank-first-n (rest-n cards 3) 2 (rank-value (card-rank (first (rest-n cards 3))))))
     true]
    [(and
      (same-rank-first-n cards 2 (rank-value (card-rank (first cards))))
      (same-rank-first-n (rest-n cards 2) 3 (rank-value (card-rank (first (rest-n cards 2))))))
     true]
    [else false]))


;; Tests:
(check-expect (is-full-house? (list (make-card 'Ace 'Club)
                       (make-card 'Ace 'Diamond)
                       (make-card 'Ace 'Heart)
                       (make-card 2 'Club)
                       (make-card 2 'Spade)))
              true)
  
(check-expect
 (is-full-house? (list (make-card 10 'Club)
                       (make-card 'Jack 'Diamond)
                       (make-card 'Queen 'Heart)
                       (make-card 'King 'Club)
                       (make-card 'Ace 'Spade)))
 false)


;; Problem 3(f)      ------------------------------------------------

;;(cards-equal? card 1 card2) Function to compare two cards (rank and suit)

;; examples

(check-expect (cards-equal? (make-card 2 'Diamond) (make-card 2 'Diamond)) true)

;;cards-equal?: (listof Card) --> bool
(define (cards-equal? card1 card2)
  (and
    (= (rank-value (card-rank card1)) (rank-value (card-rank card2)))
    (= (suit-value (card-suit card1)) (suit-value (card-suit card2)))))



;; (replace-card old-card new-card hand) produces a new list of cards
;; where all old-card is replaced with new-card.

;; Example:
(check-expect
 (replace-card (make-card 2 'Diamond) (make-card 'Ace 'Club)
   (list (make-card 2 'Club)
         (make-card 2 'Diamond)
         (make-card 'Jack 'Club)
         (make-card 9 'Spade)
         (make-card 6 'Club)))
 (list (make-card 2 'Club)
       (make-card 'Ace 'Club)
       (make-card 'Jack 'Club)
       (make-card 9 'Spade)
       (make-card 6 'Club)))

;; replace-card: Card Card (listof Card) -> (listof Card)
(define (replace-card old-card new-card hand)
  (cond
    [(empty? hand) empty]
    [(cards-equal? (first hand) old-card)
     (cons new-card
           (replace-card old-card new-card (rest hand)))]
    [else
     (cons (first hand)
           (replace-card old-card new-card (rest hand)))]))



;; Tests:
(check-expect
 (replace-card (make-card 2 'Diamond) (make-card 'Ace 'Club) empty)
 empty)

(check-expect
 (replace-card (make-card 2 'Diamond) (make-card 'Ace 'Club)
   (list (make-card 2 'Diamond)))
 (list (make-card 'Ace 'Club)))

(check-expect
 (replace-card (make-card 3 'Heart) (make-card 5 'Spade)
   (list (make-card 2 'Club)
         (make-card 3 'Heart)
         (make-card 4 'Diamond)))
 (list (make-card 2 'Club)
       (make-card 5 'Spade)
       (make-card 4 'Diamond)))