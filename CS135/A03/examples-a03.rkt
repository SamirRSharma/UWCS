;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname examples-a03) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Assignment 03 Test Cases

;; question 2a

(define p1 (make-point 1 2))
(define p2 (make-point 3 4))
(define p_zero (make-point 0 0))
(define p3 (make-point -1 3))

(check-expect (point-mult p1 p2) (make-point -5 10))
(check-expect (point-mult p1 p_zero) (make-point 0 0))
(check-expect (point-mult p1 p3) (make-point -7 1))

;; question 2b

(define p4 (make-point 4 -1))
(define p5 (make-point 2 -2)) 

(check-expect (point-div p1 p2) (make-point (/ 11 25) (/ 2 25)))
(check-expect (point-div p1 p4) (make-point (/ 2 17) (/ 9 17)))

;; question 3b

(define s1 (make-state 5 5 'North))
(define s2 (make-state 5 10 'North))
(define s3 (make-state 5 5 'South))
(define s4 (make-state 5 0 'South))
(define s5 (make-state 0 0 'North))

(check-expect (robot-ctl s1 'turn-left) (make-state 5 5 'West))
(check-expect (robot-ctl s1 'turn-right) (make-state 5 5 'East))
(check-expect (robot-ctl s1 'forward) (make-state 5 6 'North))
(check-expect (robot-ctl s2 'forward) s2)
(check-expect (robot-ctl s3 'forward) (make-state 5 4 'South))
(check-expect (robot-ctl s4 'forward) s4)
(check-expect (robot-ctl (make-state 5 5 'East) 'forward) (make-state 6 5 'East))
(check-expect (robot-ctl (make-state 5 5 'West) 'forward) (make-state 4 5 'West))
(check-expect (robot-ctl s5 'turn-left) (make-state 0 0 'West))
(check-expect (robot-ctl (make-state 0 0 'West) 'forward) (make-state 0 0 'West))

;; question 4b

(define sub-one (make-sub-mod "Primary" 10))
(define sub-two (make-sub-mod "Secondary" 5))
(define mod-one (make-mod "Module1" 20 sub-one sub-two))
(define sub-three (make-sub-mod "Third" 15))
(define mod-two (make-mod "Module2" 25 sub-three sub-two))

(check-expect (mod-weight mod-one) 35)
(check-expect (mod-weight mod-two) 45)

;; question 4c

(define robot-atom
  (make-robot "atom"
              (make-mod "Head" 10
                        (make-sub-mod "Eye" 5)
                        (make-sub-mod "Eye" 5))
              (make-mod "Arms" 100
                        (make-sub-mod "Fist-Big" 50)
                        (make-sub-mod "Fist-Small" 2))
              (make-mod "Legs" 20
                        (make-sub-mod "Foot" 10)
                        (make-sub-mod "Foot" 10))))
(define robot-smasher
  (make-robot "smasher"
              (make-mod "Head" 30
                        (make-sub-mod "Eye" 10)
                        (make-sub-mod "Eye" 10))
              (make-mod "Arms" 50
                        (make-sub-mod "Fist" 15)
                        (make-sub-mod "Fist" 15))
              (make-mod "Legs" 30
                        (make-sub-mod "Foot-Big" 8)
                        (make-sub-mod "Foot-Small" 5))))

(check-expect (predict-winner robot-atom robot-smasher) "atom")

;; question 4d

(define robot-alpha
  (make-robot "Alpha"
              (make-mod "Head-A" 20
                        (make-sub-mod "Eye-A1" 5)
                        (make-sub-mod "Eye-A2" 4))
              (make-mod "Arms-A" 50
                        (make-sub-mod "Fist-A" 30)
                        (make-sub-mod "Shield-A" 15))
              (make-mod "Legs-A" 30
                        (make-sub-mod "Foot-A1" 12)
                        (make-sub-mod "Foot-A2" 10))))

(define robot-beta
  (make-robot "Beta"
              (make-mod "Head-B" 15
                        (make-sub-mod "Eye-B1" 6)
                        (make-sub-mod "Eye-B2" 5))
              (make-mod "Arms-B" 60
                        (make-sub-mod "Fist-B" 25)
                        (make-sub-mod "Shield-B" 20))
              (make-mod "Legs-B" 35
                        (make-sub-mod "Foot-B1" 15)
                        (make-sub-mod "Foot-B2" 14))))

(check-expect
 (combine robot-alpha robot-beta)
 (make-robot "Alpha-Beta"
             (make-mod "Head-A" 20
                       (make-sub-mod "Eye-B1" 6)     
                       (make-sub-mod "Eye-B2" 5))   
             (make-mod "Arms-B" 60
                       (make-sub-mod "Fist-A" 30)   
                       (make-sub-mod "Shield-B" 20)) 
             (make-mod "Legs-B" 35
                       (make-sub-mod "Foot-B1" 15)  
                       (make-sub-mod "Foot-B2" 14)))) 