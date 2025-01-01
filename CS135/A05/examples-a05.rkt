;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname examples-a05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; [Samir Sharma] (21116578)
;; CS 135 Fall 2024
;; Assignment 05, Examples
;; ***************************************************
;;

;; A DesiredCourses is one of:
;; * empty
;; * (cons (list Str (listof Sym)) DesiredCourses)

;; Problem 1a


(define my-selections
  (list
    (list "alice1" (list 'MATH101 'CS100))
    (list "bob2" (list 'PHYS100 'CHEM100))
    (list "carol3" (list 'CS115 'CS135 'MATH135))))


(check-expect
 (missed-deadline-add my-selections "dave4")
 (list
   (list "alice1" (list 'MATH101 'CS100))
   (list "bob2" (list 'PHYS100 'CHEM100))
   (list "carol3" (list 'CS115 'CS135 'MATH135))
   (list "dave4" empty)))

(check-expect
 (missed-deadline-add my-selections "alice1")
 my-selections)

;; Problem 1b

(check-expect
 (taking-course? my-selections "carol3" 'CS135)
 true)

(check-expect
 (taking-course? my-selections "bob2" 'CS135)
 false)

(check-expect
 (taking-course? my-selections "eve5" 'CS135)
 false)

;; Problem 1c

(check-expect
 (add-course my-selections "alice1" 'CS136)
 (list
   (list "alice1" (list 'MATH101 'CS100 'CS136))
   (list "bob2" (list 'PHYS100 'CHEM100))
   (list "carol3" (list 'CS115 'CS135 'MATH135))))

(check-expect
 (add-course my-selections "alice1" 'MATH101)
 my-selections)

(check-expect
 (add-course my-selections "eve5" 'ENGL101)
 (list
   (list "alice1" (list 'MATH101 'CS100))
   (list "bob2" (list 'PHYS100 'CHEM100))
   (list "carol3" (list 'CS115 'CS135 'MATH135))
   (list "eve5" (list 'ENGL101))))

;; Problem 1d

(check-expect
 (create-classlist my-selections 'CS135)
 (list "carol3"))

(check-expect
 (create-classlist my-selections 'HIST100)
 empty)

;; Problem 3

(check-expect
 (make-symbol-lists (list 3 5 4) 'Y)
 (list (list 'Y 'Y 'Y) (list 'Y 'Y 'Y 'Y 'Y) (list 'Y 'Y 'Y 'Y)))

(check-expect
 (make-symbol-lists empty 'Z)
 empty)

(check-expect
 (make-symbol-lists (list 1 1 1) 'A)
 (list (list 'A) (list 'A) (list 'A)))

;; Problem 4a

;; No check expects only data definitions



;; Problem 4b

(check-expect
 (popular-pizza (list
    "Prof Samir"
    1
    (list
      (list "Alice" 'veggie 2)
      (list "Bob" 'meaty 3)
      (list "Carol" 'veggie 1)
      (list "Dave" 'Hawaiian 2)
      (list "Eve" 'veggie 1))))
 'veggie)

;; Problem 4c

(check-expect
 (sort-choices (list
    "Prof Samir"
    1
    (list
      (list "Alice" 'veggie 2)
      (list "Bob" 'meaty 3)
      (list "Carol" 'veggie 1)
      (list "Dave" 'Hawaiian 2)
      (list "Eve" 'veggie 1))))
 (list
    "Prof Samir"
    1
    (list
      (list "Dave" 'Hawaiian 2)
      (list "Bob" 'meaty 3)
      (list "Alice" 'veggie 2)
      (list "Carol" 'veggie 1)
      (list "Eve" 'veggie 1))))

;; Problem 4d

(check-expect
 (pizza-lookup
  (list
    (list
      "Prof C"
      103
      (list
        (list "Tom" 'meaty 2)
        (list "Sam" 'veggie 3)
        (list "Bob" 'Hawaiian 1)
        (list "Ann" 'veggie 1)
        (list "Kim" 'meaty 1)))
    (list
      "Prof D"
      104
      (list
        (list "Liz" 'Hawaiian 2)
        (list "Mike" 'veggie 1)
        (list "Nina" 'meaty 1)
        (list "Owen" 'meaty 2)
        (list "Paul" 'Hawaiian 1))))
  103
  "Sam")
 (list 'veggie 3))

(check-expect
 (pizza-lookup
  (list
    (list
      "Prof C"
      103
      (list
        (list "Tom" 'meaty 2)
        (list "Sam" 'veggie 3)
        (list "Bob" 'Hawaiian 1)
        (list "Ann" 'veggie 1)
        (list "Kim" 'meaty 1)))
    (list
      "Prof D"
      104
      (list
        (list "Liz" 'Hawaiian 2)
        (list "Mike" 'veggie 1)
        (list "Nina" 'meaty 1)
        (list "Owen" 'meaty 2)
        (list "Paul" 'Hawaiian 1))))
  104
  "Owen")
 (list 'meaty 2))

;; Problem 4e

(check-expect
 (count-slices
  (list
    (list
      "Prof C"
      103
      (list
        (list "Tom" 'meaty 2)
        (list "Sam" 'veggie 3)
        (list "Bob" 'Hawaiian 1)
        (list "Ann" 'veggie 1)
        (list "Kim" 'meaty 1)))
    (list
      "Prof D"
      104
      (list
        (list "Liz" 'Hawaiian 2)
        (list "Mike" 'veggie 1)
        (list "Nina" 'meaty 1)
        (list "Owen" 'meaty 2)
        (list "Paul" 'Hawaiian 1)))))
 (list 4 6 5)) 