;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; [Samir Sharma] (21116578)
;; CS 135 Fall 2024
;; Assignment 04, Problem 2
;; ***************************************************
;;

;; I have accomodaitons I was given a 3 day extension.






;; (listof-X-template lox) PURPOSE
;; Examples:
;;(check-expect (listof-X-template empty) ANSWER)
;;(check-expect (listof-X-template (cons X empty)) ANSWER)

;; listof-X-template: (listof X) -> Any
;;(define (listof-X-template lox)
;;  (cond [(empty? lox) ...]
;;        [(cons? lox) (... (first lox)
;;                          (listof-X-template (rest lox)))]))


;; Problem 2(a) ------------------------------------------------

;; (list-has-exactly-4-symbols lst) produces true if the list contains exactly


;; Examples:
(check-expect
 (list-has-exactly-4-symbols (cons 'One (cons "Two" (cons "Three"
   (cons 'four (cons 5 empty)))))) false)
(check-expect
 (list-has-exactly-4-symbols (cons 'One (cons 'Two (cons 'Three
   (cons 'Four (cons "Five" empty)))))) true)

;; list-has-exactly-4-symbols: (listof (anyof Num Str Bool Sym)) -> Bool
(define (list-has-exactly-4-symbols lst)
     (= (count-symbols lst) 4))
  
;; Tests:
(check-expect (list-has-exactly-4-symbols (cons 'One (cons 'Two (cons 'Three
                                          (cons 'four empty))))) true)
(check-expect (list-has-exactly-4-symbols (cons 'One (cons 'Two (cons 'Three empty)))) false)
(check-expect (list-has-exactly-4-symbols (cons 'One (cons 'Two (cons 'Three
                                          (cons 'four (cons 'five empty )))))) false)
(check-expect (list-has-exactly-4-symbols (cons 1 (cons 2 (cons 3
                                          (cons 4 empty))))) false)

;; (count-symbols lst) counts how many symbols are in the list
;; Examples:
(check-expect (count-symbols (cons 'One (cons "Two" (cons "Three"
   (cons 'four (cons 5 empty)))))) 2)

;; count-symbols: (listof (anyof Num Str Bool Sym)) -> Num
(define (count-symbols lst)
  (cond
      [(empty? lst) 0]
      [(symbol? (first lst)) (+ 1 (count-symbols (rest lst)))]
      [else (count-symbols (rest lst))]))

;; Problem 2(b) ------------------------------------------------

;; (add-only-numbers lst) produces the sum of all numbers in lst
;; Examples:
(check-expect
 (add-only-numbers (cons "Hello" (cons 2 (cons 'words
   (cons "yay" (cons 0 empty)))))) 2)
(check-expect
 (add-only-numbers (cons 1 (cons 2 (cons 3
   (cons 4 empty))))) 10)

;; add-only-numbers: (listof Any) -> Num
(define (add-only-numbers lst)
  (cond
    [(empty? lst) 0]
    [(number? (first lst)) (+ (first lst) (add-only-numbers (rest lst)))]
    [else (add-only-numbers (rest lst))]))


;; Tests:
(check-expect (add-only-numbers empty) 0)
(check-expect (add-only-numbers (cons 1 (cons 2 (cons 3 empty)))) 6)
(check-expect (add-only-numbers (cons "a" (cons "b" (cons "c" empty)))) 0)
(check-expect (add-only-numbers (cons 99 (cons "b" (cons 1 empty)))) 100)

;; Problem 2(c)  ------------------------------------------------

;; (before-after point before after lst) produces a new list where
;; inserts lists before and after point

;; Examples:
(check-expect
 (before-after "one" "two" "three"
   (cons "four" (cons "one" (cons "five" empty))))
 (cons "four" (cons "two" (cons "one"
   (cons "three" (cons "five" empty))))))

;; before-after: Str Str Str (listof Str) -> (listof Str)
(define (before-after point before after lst)
  (cond
    [(empty? lst) empty]
    [(string=? (first lst) point)
     (cons before
           (cons (first lst)
                 (cons after
                       (before-after point before after (rest lst)))))]
    [else
     (cons (first lst)
           (before-after point before after (rest lst)))]))


;; Tests:
(check-expect (before-after "a" "b" "c" empty) empty)
(check-expect (before-after "a" "b" "c" (list "x" "a" "y"))
              (list "x" "b" "a" "c" "y"))
(check-expect (before-after "a" "b" "c" (list "a" "a"))
              (list "b" "a" "c" "b" "a" "c"))

;; Problem 2(d)   ------------------------------------------------

;; (exists? val lst) produces true if val is in list

;; Examples:
(check-expect (exists? 100 (cons 'CS (cons 100 (cons "yay"
   empty)))) true)
(check-expect (exists? 100 (cons 'CS (cons "One-Hundred" (cons "yay"
   empty)))) false)

;; exists?: (anyof Num Str Sym) (listof (anyof Num Str Sym)) -> Bool
(define (exists? val lst)
  (cond
    [(empty? lst) false]
    [(and (number? val) (number? (first lst)) (= val (first lst))) true]
    [(and (string? val) (string? (first lst)) (string=? val (first lst))) true]
    [(and (symbol? val) (symbol? (first lst)) (symbol=? val (first lst))) true]
    [else (exists? val (rest lst))]))


;; Tests:
(check-expect (exists? 'CS (cons 2 (cons "100" (cons "yay"
   empty)))) false)
(check-expect (exists? "CS135" (cons "CS135" (cons "CS136" (cons "CS135"
   empty)))) true)
(check-expect (exists? 135 (cons 1 (cons 2 (cons 3
   empty)))) false)

;; Problem 2(e)    ------------------------------------------------

;; (remove-duplicates lst) produces lst with all but the last occurrence

;; Examples:
(check-expect (remove-duplicates
 (cons 'one (cons 2 (cons 'one (cons 3
   (cons 4 (cons 3 (cons 5 (cons 3
     (cons 5 empty))))))))))
 (cons 2 (cons 'one (cons 4 (cons 3 (cons 5
   empty))))))


;; remove-duplicates: (listof (anyof Num Str Sym)) -> (listof (anyof Num Str Sym))
(define (remove-duplicates lst)
  (cond
    [(empty? lst) empty]
    [(contains? (first lst) (rest lst) empty) (remove-duplicates (rest lst))]
    [else (cons (first lst) (remove-duplicates (rest lst)))]))

;; contains?: anyof Num Str Sym (listof (anyof Num Str Sym)) (listof (anyof Num Str Sym)) -> Bool
(define (contains? elem lst seen)
  (cond
    [(empty? lst) false]
    [(and (number? elem) (number? (first lst)) (= elem (first lst))) true]
    [(and (string? elem) (string? (first lst)) (string=? elem (first lst))) true]
    [(and (symbol? elem) (symbol? (first lst)) (symbol=? elem (first lst))) true]
    [else (contains? elem (rest lst) (cons (first lst) seen))]))


;; Tests:
(check-expect (remove-duplicates
               (cons 1 (cons 2 (cons 3 (cons 2 (cons 1 empty))))))
               (cons 3 (cons 2 (cons 1 empty))))
(check-expect (remove-duplicates
               (cons "a" (cons "b" (cons "a" (cons "c" empty)))))
               (cons "b" (cons "a" (cons "c" empty))))
(check-expect (remove-duplicates empty) empty)
