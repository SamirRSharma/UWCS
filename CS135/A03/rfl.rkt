;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rfl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; REMINDER THAT I HAVE ACCOMODATIONS THATS WHY I A SUBMITING PAST 9:00PM. I FILLED OUT THE FORM AND
;; WAS TOLD I HAVE UNTIL FRIDAY 9PM TO SUBMIT
;; ANY QUESTIONS PLEASE EMAIL ME OR TALK TO KAREN ANDERSON

;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 03, Problem 4a
;; ***************************************************
;;

(define-struct sub-mod (name weight))
;; A Sub-Mod is a (make-sub-mod Str Num)
;; Requires: weight > 0

(define-struct mod (name frame-wt primary secondary))
;; A Mod is a (make-mod Str Num Sub-Mod Sub-Mod)
;; Requires: frame-wt > 0, primary weight <= secondary weight

(define-struct robot (name head arms legs))
;; A Robot is a (make-robot Str Mod Mod Mod)

;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 03, Problem 4b
;; ***************************************************
;;


;; (mod-weight modules) produces the total weight of the module 
;; Examples:
(check-expect (mod-weight (make-mod "Mod1" 10 (make-sub-mod "Sub1" 1) (make-sub-mod "Sub2" 1))) 12)

;; mod-weight: Mod -> PositiveNum
(define (mod-weight modules)
  (+ (mod-frame-wt modules)
     (sub-mod-weight (mod-primary modules))
     (sub-mod-weight (mod-secondary modules))))

;; tests

(check-expect (mod-weight (make-mod "Mod2" 10 (make-sub-mod "Sub1" 3) (make-sub-mod "Sub2" 5))) 18)
(check-expect (mod-weight (make-mod "Mod3" 10 (make-sub-mod "Sub3" 2) (make-sub-mod "Sub4" 2))) 14)



;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 03, Problem 4c
;; ***************************************************
;;


;; (robot-weight modules) produces the total weight
              
;; Example:
(check-expect (robot-weight
               (make-robot "Example-Robot"
    (make-mod "Head" 10
      (make-sub-mod "Eye1" 5)
      (make-sub-mod "Eye2" 5))
    (make-mod "Arms" 10
      (make-sub-mod "Fist1" 10)
      (make-sub-mod "Fist2" 10))
    (make-mod "Legs" 10
      (make-sub-mod "Foot1" 10)
      (make-sub-mod "Foot2" 10))))
              80)

;; robot-weight: Robot -> PositiveNum

(define (robot-weight robot)
  (+
     (mod-frame-wt (robot-head robot))
     (sub-mod-weight (mod-primary (robot-head robot)))
     (sub-mod-weight (mod-secondary (robot-head robot)))
     
     (mod-frame-wt (robot-arms robot))
     (sub-mod-weight (mod-primary (robot-arms robot)))
     (sub-mod-weight (mod-secondary (robot-arms robot)))

     (mod-frame-wt (robot-legs robot))
     (sub-mod-weight (mod-primary (robot-legs robot)))
     (sub-mod-weight (mod-secondary (robot-legs robot)))))




;; (predict-winner robot1 robot2) produces the name of the heavier robot, but first name if its a tie

;; Examples:
(check-expect (predict-winner
               (make-robot "RobotOne"
    (make-mod "Head" 10
      (make-sub-mod "Eye1" 10)
      (make-sub-mod "Eye2" 10))
    (make-mod "Arms" 10
      (make-sub-mod "Fist1" 10)
      (make-sub-mod "Fist2" 10))
    (make-mod "Legs" 10
      (make-sub-mod "Foot1" 10)
      (make-sub-mod "Foot2" 10)))
               (make-robot "RobotTwo"
    (make-mod "Head" 10
      (make-sub-mod "Eye1" 5)
      (make-sub-mod "Eye2" 5))
    (make-mod "Arms" 10
      (make-sub-mod "Fist1" 10)
      (make-sub-mod "Fist2" 10))
    (make-mod "Legs" 10
      (make-sub-mod "Foot1" 10)
      (make-sub-mod "Foot2" 10))))
              "RobotOne")

;; predict-winner: Robot Robot -> Str
(define (predict-winner robot1 robot2)
  (cond
    [(> (robot-weight robot1) (robot-weight robot2)) (robot-name robot1)]
    [(< (robot-weight robot1) (robot-weight robot2)) (robot-name robot2)]
    [else (robot-name robot1)]))

;; Examples and Tests for predict-winner:


(check-expect (predict-winner
               (make-robot "RobotOne"
    (make-mod "Head" 10
      (make-sub-mod "Eye1" 10)
      (make-sub-mod "Eye2" 10))
    (make-mod "Arms" 10
      (make-sub-mod "Fist1" 10)
      (make-sub-mod "Fist2" 10))
    (make-mod "Legs" 10
      (make-sub-mod "Foot1" 10)
      (make-sub-mod "Foot2" 10)))
               (make-robot "RobotTwo"
    (make-mod "Head" 10
      (make-sub-mod "Eye1" 10)
      (make-sub-mod "Eye2" 10))
    (make-mod "Arms" 10
      (make-sub-mod "Fist1" 10)
      (make-sub-mod "Fist2" 10))
    (make-mod "Legs" 10
      (make-sub-mod "Foot1" 10)
      (make-sub-mod "Foot2" 10))))
              "RobotOne")

(check-expect (predict-winner
               (make-robot "RobotOne"
    (make-mod "Head" 10
      (make-sub-mod "Eye1" 10)
      (make-sub-mod "Eye2" 10))
    (make-mod "Arms" 10
      (make-sub-mod "Fist1" 10)
      (make-sub-mod "Fist2" 6))
    (make-mod "Legs" 10
      (make-sub-mod "Foot1" 10)
      (make-sub-mod "Foot2" 10)))
               (make-robot "RobotTwo"
    (make-mod "Head" 10
      (make-sub-mod "Eye1" 5)
      (make-sub-mod "Eye2" 5))
    (make-mod "Arms" 10
      (make-sub-mod "Fist1" 20)
      (make-sub-mod "Fist2" 10))
    (make-mod "Legs" 10
      (make-sub-mod "Foot1" 27)
      (make-sub-mod "Foot2" 10))))
              "RobotTwo")

;;
;; ***************************************************
;; Samir Sharma (21116578)
;; CS 135 Fall 2024
;; Assignment 03, Problem 4d
;; ***************************************************
;;


;; (combine robot1 robot2) produces a Robot that combines robot1 and robot2
;; A Robot is a (make-robot Str Mod Mod Mod)
;; A Sub-Mod is a (make-sub-mod Str Num)
;; Requires: frame-wt > 0, primary weight <= secondary weight


;; Examples:
(check-expect (combine
               (make-robot "Atom"
                 (make-mod "Head-A" 10
                   (make-sub-mod "Eye-A" 5)
                   (make-sub-mod "Eye-A" 5))
                 (make-mod "Arms-A" 100
                   (make-sub-mod "Fist-A-Big" 50)
                   (make-sub-mod "Fist-A-Small" 2))
                 (make-mod "Legs-A" 20
                   (make-sub-mod "Foot-A" 10)
                   (make-sub-mod "Foot-A" 10)))
               (make-robot "Smasher"
                 (make-mod "Head-S" 30
                   (make-sub-mod "Eye-S" 10)
                   (make-sub-mod "Eye-S" 10))
                 (make-mod "Arms-S" 50
                   (make-sub-mod "Fist-S" 15)
                   (make-sub-mod "Fist-S" 15))
                 (make-mod "Legs-S" 30
                   (make-sub-mod "Foot-S-Big" 8)
                   (make-sub-mod "Foot-S-Small" 5))))
              (make-robot "Atom-Smasher"
                (make-mod "Head-S" 30
                  (make-sub-mod "Eye-S" 10)
                  (make-sub-mod "Eye-S" 10))
                (make-mod "Arms-A" 100
                  (make-sub-mod "Fist-A-Big" 50)
                  (make-sub-mod "Fist-S" 15))
                (make-mod "Legs-S" 30
                  (make-sub-mod "Foot-A" 10)
                  (make-sub-mod "Foot-A" 10))))

;; combine: Robot Robot -> Robot
(define (combine robot1 robot2)
  (make-robot
    (string-append (robot-name robot1) "-" (robot-name robot2))
    (combine-modules (robot-head robot1) (robot-head robot2))
    (combine-modules (robot-arms robot1) (robot-arms robot2))
    (combine-modules (robot-legs robot1) (robot-legs robot2))))


;; Design Recipe for `combine-modules`:
;; Examples:
(check-expect (combine-modules
               (make-mod "Arms-A" 100
                 (make-sub-mod "Fist-A-Big" 50)
                 (make-sub-mod "Fist-A-Small" 2))
               (make-mod "Arms-S" 50
                 (make-sub-mod "Fist-S" 15)
                 (make-sub-mod "Fist-S" 15)))
              (make-mod "Arms-A" 100
                (make-sub-mod "Fist-A-Big" 50)
                (make-sub-mod "Fist-S" 15)))

;; combine-modules: Mod Mod -> Mod
(define (combine-modules module1 module2)
  (make-mod
    (cond
      [(> (mod-frame-wt module1) (mod-frame-wt module2)) (mod-name module1)]
      [(< (mod-frame-wt module1) (mod-frame-wt module2)) (mod-name module2)]
      [else (mod-name module1)])  
    (cond
      [(> (mod-frame-wt module1) (mod-frame-wt module2)) (mod-frame-wt module1)]
      [(< (mod-frame-wt module1) (mod-frame-wt module2)) (mod-frame-wt module2)]
      [else (mod-frame-wt module1)])  
    (get-primary-sub-mod module1 module2)
    (get-secondary-sub-mod module1 module2)))


;; (get-primary-sub-mod module1 module2) returns the heaviest sub-module from both modules.
;; Examples:
(check-expect (get-primary-sub-mod
               (make-mod "Arms-A" 100
                 (make-sub-mod "Fist-A-Big" 50)
                 (make-sub-mod "Fist-A-Small" 2))
               (make-mod "Arms-S" 50
                 (make-sub-mod "Fist-S" 15)
                 (make-sub-mod "Fist-S" 15)))
              (make-sub-mod "Fist-A-Big" 50))

;; get-primary-sub-mod: Mod Mod -> Sub-Mod
(define (get-primary-sub-mod module1 module2)
  (find-heaviest-sub-mod
    (mod-primary module1)
    (mod-secondary module1)
    (mod-primary module2)
    (mod-secondary module2)))



;; (get-secondary-sub-mod module1 module2) returns the second heaviest sub-module from both modules.
;; Examples:
(check-expect (get-secondary-sub-mod
               (make-mod "Arms-A" 100
                 (make-sub-mod "Fist-A-Big" 50)
                 (make-sub-mod "Fist-A-Small" 2))
               (make-mod "Arms-S" 50
                 (make-sub-mod "Fist-S" 15)
                 (make-sub-mod "Fist-S" 15)))
              (make-sub-mod "Fist-S" 15))

;; get-secondary-sub-mod: Mod Mod -> Sub-Mod
(define (get-secondary-sub-mod module1 module2)
  (find-second-heaviest-sub-mod
    (mod-primary module1)
    (mod-secondary module1)
    (mod-primary module2)
    (mod-secondary module2)
    (get-primary-sub-mod module1 module2)))



;; (find-heaviest-sub-mod sub-mod1 sub-mod2 sub-mod3 sub-mod4) returns the heaviest
;; sub-module from the four.
;; Examples:
(check-expect (find-heaviest-sub-mod
               (make-sub-mod "Fist-A-Big" 50)
               (make-sub-mod "Fist-A-Small" 2)
               (make-sub-mod "Fist-S" 15)
               (make-sub-mod "Foot-S-Big" 8))
              (make-sub-mod "Fist-A-Big" 50))

;; find-heaviest-sub-mod: Sub-Mod Sub-Mod Sub-Mod Sub-Mod -> Sub-Mod
(define (find-heaviest-sub-mod sub-mod1 sub-mod2 sub-mod3 sub-mod4)
  (cond
    [(and (>= (sub-mod-weight sub-mod1) (sub-mod-weight sub-mod2))
          (>= (sub-mod-weight sub-mod1) (sub-mod-weight sub-mod3))
          (>= (sub-mod-weight sub-mod1) (sub-mod-weight sub-mod4))) sub-mod1]
    [(and (>= (sub-mod-weight sub-mod2) (sub-mod-weight sub-mod1))
          (>= (sub-mod-weight sub-mod2) (sub-mod-weight sub-mod3))
          (>= (sub-mod-weight sub-mod2) (sub-mod-weight sub-mod4))) sub-mod2]
    [(and (>= (sub-mod-weight sub-mod3) (sub-mod-weight sub-mod1))
          (>= (sub-mod-weight sub-mod3) (sub-mod-weight sub-mod2))
          (>= (sub-mod-weight sub-mod3) (sub-mod-weight sub-mod4))) sub-mod3]
    [else sub-mod4]))


;; (find-second-heaviest-sub-mod sub-mod1 sub-mod2 sub-mod3 sub-mod4 heaviest-sub-mod) returns
;;the second heaviest sub-module from the four sub-mods.
;; Examples:
(check-expect (find-second-heaviest-sub-mod
               (make-sub-mod "Fist-A-Big" 50)
               (make-sub-mod "Fist-A-Small" 2)
               (make-sub-mod "Fist-S" 15)
               (make-sub-mod "Foot-S-Big" 8)
               (make-sub-mod "Fist-A-Big" 50))
              (make-sub-mod "Fist-S" 15))

;; find-second-heaviest-sub-mod: Sub-Mod Sub-Mod Sub-Mod Sub-Mod Sub-Mod -> Sub-Mod
(define (find-second-heaviest-sub-mod sub-mod1 sub-mod2 sub-mod3 sub-mod4 heaviest-sub-mod)
  (cond
    [(and (string=? (sub-mod-name sub-mod1) (sub-mod-name heaviest-sub-mod))
          (= (sub-mod-weight sub-mod1) (sub-mod-weight heaviest-sub-mod)))
     (find-heaviest-of-three sub-mod2 sub-mod3 sub-mod4)]
    [(and (string=? (sub-mod-name sub-mod2) (sub-mod-name heaviest-sub-mod))
          (= (sub-mod-weight sub-mod2) (sub-mod-weight heaviest-sub-mod)))
     (find-heaviest-of-three sub-mod1 sub-mod3 sub-mod4)]
    [(and (string=? (sub-mod-name sub-mod3) (sub-mod-name heaviest-sub-mod))
          (= (sub-mod-weight sub-mod3) (sub-mod-weight heaviest-sub-mod)))
     (find-heaviest-of-three sub-mod1 sub-mod2 sub-mod4)]
    [(and (string=? (sub-mod-name sub-mod4) (sub-mod-name heaviest-sub-mod))
          (= (sub-mod-weight sub-mod4) (sub-mod-weight heaviest-sub-mod)))
     (find-heaviest-of-three sub-mod1 sub-mod2 sub-mod3)]
    [else sub-mod1]))


;; (find-heaviest-of-three sub-moda sub-modb sub-modc) returns the heaviest sub-module
;; from the three sub-modules.
;; Examples:
(check-expect (find-heaviest-of-three
               (make-sub-mod "Fist-S" 15)
               (make-sub-mod "Foot-S-Big" 8)
               (make-sub-mod "Foot-S-Small" 5))
              (make-sub-mod "Fist-S" 15))

;; find-heaviest-of-three: Sub-Mod Sub-Mod Sub-Mod -> Sub-Mod
(define (find-heaviest-of-three sub-moda sub-modb sub-modc)
  (cond
    [(and (>= (sub-mod-weight sub-moda) (sub-mod-weight sub-modb))
          (>= (sub-mod-weight sub-moda) (sub-mod-weight sub-modc))) sub-moda]
    [(and (>= (sub-mod-weight sub-modb) (sub-mod-weight sub-moda))
          (>= (sub-mod-weight sub-modb) (sub-mod-weight sub-modc))) sub-modb]
    [else sub-modc]))








;; Tests for overall function

(check-expect (combine
               (make-robot "Bot1"
                 (make-mod "Head" 10
                   (make-sub-mod "Eye" 5)
                   (make-sub-mod "Ear" 5))
                 (make-mod "Arms" 20
                   (make-sub-mod "Fist" 10)
                   (make-sub-mod "Palm" 10))
                 (make-mod "Legs" 30
                   (make-sub-mod "Foot" 15)
                   (make-sub-mod "Toe" 15)))
               (make-robot "Bot2"
                 (make-mod "Head" 10
                   (make-sub-mod "Eye" 5)
                   (make-sub-mod "Ear" 5))
                 (make-mod "Arms" 20
                   (make-sub-mod "Fist" 10)
                   (make-sub-mod "Palm" 10))
                 (make-mod "Legs" 30
                   (make-sub-mod "Foot" 15)
                   (make-sub-mod "Toe" 15))))
              (make-robot "Bot1-Bot2"
                (make-mod "Head" 10
                  (make-sub-mod "Eye" 5)
                  (make-sub-mod "Ear" 5))
                (make-mod "Arms" 20
                  (make-sub-mod "Fist" 10)
                  (make-sub-mod "Palm" 10))
                (make-mod "Legs" 30
                  (make-sub-mod "Foot" 15)
                  (make-sub-mod "Toe" 15))))


(check-expect (combine
               (make-robot "LightBot"
                 (make-mod "Head" 1
                   (make-sub-mod "LightSensor" 1)
                   (make-sub-mod "LightVision" 1))
                 (make-mod "Arms" 2
                   (make-sub-mod "LightClaw" 1)
                   (make-sub-mod "LightGrip" 1))
                 (make-mod "Legs" 3
                   (make-sub-mod "LightKnee" 1)
                   (make-sub-mod "LightFoot" 1)))
               (make-robot "HeavyBot"
                 (make-mod "Head" 50
                   (make-sub-mod "HeavySensor" 30)
                   (make-sub-mod "HeavyVision" 25))
                 (make-mod "Arms" 100
                   (make-sub-mod "HeavyClaw" 75)
                   (make-sub-mod "HeavyGrip" 70))
                 (make-mod "Legs" 200
                   (make-sub-mod "HeavyKnee" 150)
                   (make-sub-mod "HeavyFoot" 120))))
              (make-robot "LightBot-HeavyBot"
                (make-mod "Head" 50
                  (make-sub-mod "HeavySensor" 30)
                  (make-sub-mod "HeavyVision" 25))
                (make-mod "Arms" 100
                  (make-sub-mod "HeavyClaw" 75)
                  (make-sub-mod "HeavyGrip" 70))
                (make-mod "Legs" 200
                  (make-sub-mod "HeavyKnee" 150)
                  (make-sub-mod "HeavyFoot" 120))))


(check-expect (combine
               (make-robot "MixBot1"
                 (make-mod "Head-A" 20
                   (make-sub-mod "Eye-A" 8)
                   (make-sub-mod "Ear-A" 7))
                 (make-mod "Arms-A" 35
                   (make-sub-mod "Claw-A" 18)
                   (make-sub-mod "Grip-A" 15))
                 (make-mod "Legs-A" 40
                   (make-sub-mod "Foot-A" 20)
                   (make-sub-mod "Toe-A" 18)))
               (make-robot "MixBot2"
                 (make-mod "Head-B" 25
                   (make-sub-mod "Eye-B" 12)
                   (make-sub-mod "Ear-B" 10))
                 (make-mod "Arms-B" 30
                   (make-sub-mod "Claw-B" 16)
                   (make-sub-mod "Grip-B" 14))
                 (make-mod "Legs-B" 45
                   (make-sub-mod "Foot-B" 25)
                   (make-sub-mod "Toe-B" 20))))
              (make-robot "MixBot1-MixBot2"
                (make-mod "Head-B" 25
                  (make-sub-mod "Eye-B" 12)
                  (make-sub-mod "Ear-B" 10))
                (make-mod "Arms-A" 35
                  (make-sub-mod "Claw-A" 18)
                  (make-sub-mod "Claw-B" 16))
                (make-mod "Legs-B" 45
                  (make-sub-mod "Foot-B" 25)
                  (make-sub-mod "Foot-A" 20))))



