;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pizza-party) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; [Samir Sharma] (21116578)
;; CS 135 Fall 2024
;; Assignment 05, Problem 4
;; ***************************************************
;;

;; Problem 4a --------------------------------------------------------------------------------------


;; A StudentChoice is:
;; * (list Str Sym Nat)
;; Requires: slices >= 0

(define (student-name StudentChoice) (first StudentChoice))
(define (student-pizza StudentChoice) (second StudentChoice))
(define (student-slices StudentChoice) (third StudentChoice))

;; studentchoice-template: StudentChoice -> Any
(define (studentchoice-template StudentChoice)
  (... (student-name StudentChoice)
       (student-pizza StudentChoice)
       (student-slices StudentChoice)))


;; A Section is:
;; * (list Str PosInt (listof StudentChoice))
;; Requires: section-number > 0

(define (section-instructor Section) (first Section))
(define (section-number Section) (second Section))
(define (section-choices Section) (third Section))

;; section-template: Section -> Any
(define (section-template Section)
  (... (section-instructor Section)
       (section-number Section)
       (studentchoice-list-template (section-choices Section))))


;; A Course is:
;; * empty
;; * (listof Section)

;; course-template: Course -> Any
(define (course-template c)
  (cond
    [(empty? c) (...)]
    [else
     (... (section-template (first c))
          (course-template (rest c)))]))


;; studentchoice-list-template: (listof StudentChoice) -> Any
(define (studentchoice-list-template loc)
  (cond
    [(empty? loc) (...)]
    [else
     (... (studentchoice-template (first loc))
          (studentchoice-list-template (rest loc)))]))

;; Problem 4b --------------------------------------------------------------------------------------


;; (popular-pizza section) produces the symbol of the most popular pizza type
;; based on amount of students that choose it

;; example

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

;; (popular-pizza section): Section -> Sym
(define (popular-pizza section)
  (count-pizza (section-choices section) 0 0 0))

;; tests


(check-expect
 (popular-pizza (list
    "Prof Samir"
    1
    (list
      (list "Alice" 'meaty 2)
      (list "Bob" 'meaty 3)
      (list "Carol" 'meaty 1)
      (list "Dave" 'meaty 2)
      (list "Eve" 'veggie 1))))
 'meaty)

(check-expect
 (popular-pizza (list
    "Prof Samir"
    1
    (list
      (list "Alice" 'veggie 2)
      (list "Bob" 'veggie 3)
      (list "Carol" 'Hawaiian 1)
      (list "Dave" 'Hawaiian 2)
      (list "Eve" 'Hawaiian 1))))
 'Hawaiian)




;; (count-pizza choices veggie-count meaty-count hawaiian-count)
;; wrapper that returns the most popular pizza type

;; Example
(check-expect (count-pizza
  (list
   (list "Alice" 'veggie 2)
  (list "Bob" 'meaty 3)
  (list "Carol" 'veggie 1)
 (list "Dave" 'Hawaiian 2)
  (list "Eve" 'veggie 1))
 0 0 0)
 'veggie)

;; count-pizza: (listof StudentChoice) Nat Nat Nat -> Sym
(define (count-pizza choices veggie-count meaty-count hawaiian-count)
  (cond
    [(empty? choices)
     (cond
       [(and (> veggie-count meaty-count) (> veggie-count hawaiian-count)) 'veggie]
       [(and (> meaty-count veggie-count) (> meaty-count hawaiian-count)) 'meaty]
       [else 'Hawaiian])]
    [else (add-pizza choices veggie-count meaty-count hawaiian-count)
     ]))



;; (add-pizza choices veggie-count meaty-count hawaiian-count) adds the pizza and calls count-pizza

;; example

(check-expect (add-pizza
  (list
   (list "Alice" 'veggie 2)
  (list "Bob" 'meaty 3)
  (list "Carol" 'veggie 1)
 (list "Dave" 'Hawaiian 2)
  (list "Eve" 'veggie 1))
 0 0 0)
 'veggie)

;; count-pizza: (listof StudentChoice) Nat Nat Nat -> List
(define (add-pizza choices veggie-count meaty-count hawaiian-count)
  (cond
       [(symbol=? (student-pizza (first choices)) 'veggie)
        (count-pizza (rest choices) (+ veggie-count 1) meaty-count hawaiian-count)]
       [(symbol=? (student-pizza (first choices)) 'meaty)
        (count-pizza (rest choices) veggie-count (+ meaty-count 1) hawaiian-count)]
       [else  
        (count-pizza (rest choices) veggie-count meaty-count (+ hawaiian-count 1))]))



;; Problem 4c --------------------------------------------------------------------------------------


;; (sort-choices s) produces the same Section s where the list of StudentChoices
;; is ordered by pizza type.

;; Examples:
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

;; sort-choices: Section -> Section
(define (sort-choices s)
  (list (section-instructor s)
        (section-number s)
        (sort-choices-list (section-choices s))))

;; tests


(check-expect
 (sort-choices (list
    "Prof Samir"
    1
    (list
      (list "Dave" 'meaty 5)
      (list "Bob" 'meaty 4)
      (list "Alice" 'meaty 3)
      (list "Carol" 'meaty 2)
      (list "Eve" 'meaty 1))))
 (list
    "Prof Samir"
    1
    (list
      (list "Alice" 'meaty 3)
      (list "Bob" 'meaty 4)
      (list "Carol" 'meaty 2)
      (list "Dave" 'meaty 5)
      (list "Eve" 'meaty 1))))
 

(check-expect
 (sort-choices (list
    "Prof Johnson"
    2
    (list
      (list "Fiona" 'meaty 1)
      (list "George" 'Hawaiian 2)
      (list "Hannah" 'veggie 3)
      (list "Ivan" 'meaty 2)
      (list "Jack" 'veggie 2)
      (list "Karen" 'Hawaiian 1))))
 (list
   "Prof Johnson"
   2
    (list
      (list "George" 'Hawaiian 2)
      (list "Karen" 'Hawaiian 1)
      (list "Fiona" 'meaty 1)
      (list "Ivan" 'meaty 2)
      (list "Hannah" 'veggie 3)
      (list "Jack" 'veggie 2))))







 




;; (sort-choices-list loc) produces sorted list of StudentChoice

;; Examples:


;; sort-choices-list: (listof StudentChoice) -> (listof StudentChoice)
(define (sort-choices-list loc)
  (cond
    [(empty? loc) empty]
    [else (insert-choice (first loc)
                         (sort-choices-list (rest loc)))]))



;; (insert-choice c sloc) inserts StudentChoice c into the sorted list

;; Example:

;; insert-choice: StudentChoice (listof StudentChoice) -> (listof StudentChoice)
(define (insert-choice c sloc)
  (cond
    [(empty? sloc) (list c)]
    [(choices<= c (first sloc)) (cons c sloc)]
    [else (cons (first sloc) (insert-choice c (rest sloc)))]))




;; (choices<= c1 c2) produces true if c1 should come before c2

;; Examples:
(check-expect (choices<= (list "Alice" 'Hawaiian 2) (list "Bob" 'meaty 3)) true)


;; choices<=: StudentChoice StudentChoice -> Bool
(define (choices<= c1 c2)
  (cond
    [(< (pizza-rank (student-pizza c1))
        (pizza-rank (student-pizza c2)))
     true]
    [(> (pizza-rank (student-pizza c1))
        (pizza-rank (student-pizza c2)))
     false]
    [else
     (string<=? (student-name c1)
                (student-name c2))]))


;; (pizza-rank pizza-type) produces a numeric rank for pizza-type

;; Example:
(check-expect (pizza-rank 'Hawaiian) 1)

;; pizza-rank: Sym -> Nat
(define (pizza-rank pizza-type)
  (cond
    [(symbol=? pizza-type 'Hawaiian) 1]
    [(symbol=? pizza-type 'meaty) 2]
    [else 3]))



















;; Problem 4d --------------------------------------------------------------------------------------


;; (pizza-lookup course section-num name)
;; produces list containing pizza type and clice count for student

;;example
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


;; (pizza-lookup course section-num name): Course PosInt Str -> (list Sym Nat)
(define (pizza-lookup course section-num name)
  (cond
   [(= (section-number (first course)) section-num)
    (person-lookup (section-choices (first course)) name)]
   (else (pizza-lookup (rest course) section-num name))))

;; tests 
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
  "Ann")
 (list 'veggie 1))

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
  "Tom")
 (list 'meaty 2))

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
  "Liz")
 (list 'Hawaiian 2))



;; (person-lookup choices name) produces the pizza type and amount for certin section choices

;; example;

(check-expect (person-lookup
               (list
        (list "Liz" 'Hawaiian 2)
        (list "Mike" 'veggie 1)
        (list "Nina" 'meaty 1)
        (list "Owen" 'meaty 2)
        (list "Paul" 'Hawaiian 1))
  "Paul")
  (list 'Hawaiian 1))
               
;; (person-lookup choices name): (listof StudentChoice) -> (list Sym Nat)
(define (person-lookup choices name)
  (cond
    [(string=? (first (first choices)) name)
     (list (second (first choices)) (third (first choices)))]
    [else (person-lookup (rest choices) name)]
  ))

;; Problem 4e --------------------------------------------------------------------------------------


;; (count-slices-section choices) produces list of total student choices

;; example
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

;; count-slices: Course -> (list Nat Nat Nat)
(define (count-slices course)
  (cond
    [(empty? course) (list 0 0 0)]
    [else
     (sum-counts (count-slices-section (section-choices (first course)))
                 (count-slices (rest course)))]))

;; tests
(check-expect
 (count-slices
  (list
    (list
      "Prof C"
      103
      (list
        (list "Tom" 'meaty 1)
        (list "Sam" 'veggie 1)
        (list "Bob" 'Hawaiian 1)
        (list "Ann" 'veggie 1)
        (list "Kim" 'meaty 1)))
    (list
      "Prof D"
      104
      (list
        (list "Liz" 'Hawaiian 1)
        (list "Mike" 'veggie 1)
        (list "Nina" 'meaty 1)
        (list "Owen" 'meaty 1)
        (list "Paul" 'Hawaiian 1)))))
 (list 3 4 3))

(check-expect
 (count-slices
  (list
    (list
      "Prof C"
      103
      (list
        (list "Tom" 'meaty 1)
        (list "Sam" 'meaty 1)
        (list "Bob" 'meaty 1)
        (list "Ann" 'meaty 1)
        (list "Kim" 'meaty 1)))
    (list
      "Prof D"
      104
      (list
        (list "Liz" 'meaty 1)
        (list "Mike" 'meaty 1)
        (list "Nina" 'meaty 1)
        (list "Owen" 'meaty 1)
        (list "Paul" 'meaty 1)))))
 (list 0 10 0))


;; (count-slices-choice sc)
;; produces a fixed-length list of three items based on slice counts for section

;; Example:
(check-expect
 (count-slices-section
      (list
        (list "Liz" 'Hawaiian 2)
        (list "Mike" 'veggie 1)
        (list "Nina" 'meaty 1)
        (list "Owen" 'meaty 2)
        (list "Paul" 'Hawaiian 1)))
 (list 3 3 1))


;; count-slices-choices: (listof StudentChoice) -> (list Nat Nat Nat)
(define (count-slices-section choices)
  (cond
    [(empty? choices) (list 0 0 0)]
    [else
     (sum-counts (count-slices-choice (first choices))
                 (count-slices-section (rest choices)))]))


;; (count-slices-choice sc)
;; produces a fixed-length list of three items based on slice counts for individual StudentChoice

;; Example:

(check-expect
 (count-slices-choice (list "Paul" 'Hawaiian 1)) 
 (list 1 0 0))


;; count-slices-choice: StudentChoice -> (list Nat Nat Nat)
(define (count-slices-choice sc)
  (cond
    [(symbol=? (student-pizza sc) 'Hawaiian) (list (student-slices sc) 0 0)]
    [(symbol=? (student-pizza sc) 'meaty) (list 0 (student-slices sc) 0)]
    [(symbol=? (student-pizza sc) 'veggie) (list 0 0 (student-slices sc))]
    [else (list 0 0 0)]))





;; (sum-counts counts1 counts2) adds two counts

;; Example:
(check-expect (sum-counts (list 1 2 3) (list 4 5 6)) (list 5 7 9))

;; sum-counts: (list Nat Nat Nat) (list Nat Nat Nat) -> (list Nat Nat Nat)
(define (sum-counts counts1 counts2)
  (list (+ (first counts1) (first counts2))
        (+ (second counts1) (second counts2))
        (+ (third counts1) (third counts2))))

