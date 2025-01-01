;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot-nav) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; **********************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 03, Problem 3
;; **********************************************

(define-struct state (x y dir))
;; A State is a (make-state Nat Nat String)
;; Requires: - 0 ≤ x ≤ 10 - 0 ≤ y ≤ 10
;; Direction to be: 'North, 'East, 'South, 'West

;; A Command to be: 'turn-left, 'turn-right, 'forward

;; (robot-ctl s cmd) consumes a State `current-state` and a Command `command` and produces a new state
;; examples:
(check-expect (robot-ctl (make-state 1 1 'South) 'forward) (make-state 1 0 'South))
(check-expect (robot-ctl (make-state 3 3 'West) 'turn-left) (make-state 3 3 'South))

;; robot-ctl: State Command -> State
(define (robot-ctl current-state command)
  (cond
    [(symbol=? command 'turn-left)
     (make-state
      (state-x current-state) (state-y current-state) (turn-left (state-dir current-state)))]
    [(symbol=? command 'turn-right)
     (make-state
      (state-x current-state) (state-y current-state) (turn-right (state-dir current-state)))]
    [(symbol=? command 'forward)
     (move-forward current-state)]))


;; tests
(check-expect (robot-ctl (make-state 5 5 'North) 'turn-right) (make-state 5 5 'East))
(check-expect (robot-ctl (make-state 0 0 'South) 'forward) (make-state 0 0 'South)) ; Can't move south
(check-expect (robot-ctl (make-state 10 10 'South) 'forward) (make-state 10 9 'South))
(check-expect (robot-ctl (make-state 9 9 'West) 'forward) (make-state 8 9 'West))




;; (turn-left direction) produces the new direction after turning left.
;; turn-left: Direction -> Direction
;; Example:
(check-expect (turn-left 'North) 'West)

(define (turn-left direction)
  (cond
    [(symbol=? direction 'North) 'West]
    [(symbol=? direction 'West) 'South]
    [(symbol=? direction 'South) 'East]
    [(symbol=? direction 'East) 'North]))

;; (turn-right direction) produces the new direction after turning right.
;; turn-right: Direction -> Direction
;; Example:
(check-expect (turn-right-helper 'North) 'East)

(define (turn-right direction)
  (cond
    [(symbol=? direction 'North) 'East]
    [(symbol=? direction 'East) 'South]
    [(symbol=? direction 'South) 'West]
    [(symbol=? direction 'West) 'North]))

;; (move-forward current-state produces a new State after moving forward
;; move-forward: State -> State
;; Example:
(check-expect (move-forward (make-state 5 5 'North)) (make-state 5 6 'North))


(define (move-forward current-state)
  (cond
    [(symbol=? (state-dir current-state) 'North)
     (cond
       [(<= (state-y current-state) 10)
        (make-state
         (state-x current-state) (+ (state-y current-state) 1) (state-dir current-state))]
       [else current-state])]
    [(symbol=? (state-dir current-state) 'South)
     (cond
       [(>= (state-y current-state) 0)
        (make-state
         (state-x current-state) (- (state-y current-state) 1) (state-dir current-state))]
       [else current-state])]
    [(symbol=? (state-dir current-state) 'East)
     (cond
       [(< (state-x current-state) 10)
        (make-state
         (+ (state-x current-state) 1) (state-y current-state) (state-dir current-state))]
       [else current-state])]
    [(symbol=? (state-dir current-state) 'West)
     (cond
       [(> (state-x current-state) 0)
        (make-state
         (- (state-x current-state) 1) (state-y current-state) (state-dir current-state))]
       [else current-state])]))

