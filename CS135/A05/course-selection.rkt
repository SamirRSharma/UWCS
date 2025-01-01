;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Untitledcourse-selection) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; [Samir Sharma] (21116578)
;; CS 135 Fall 2024
;; Assignment 05, Problem 2
;; ***************************************************
;;

;; A DesiredCourses is one of:
;; * empty
;; * (cons (list Str (listof Sym)) DesiredCourses)

(define selections
(list
(list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
(list "alice1" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
(list "abc123" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
(list "s576shar" (list 'CLAS104 'LS201 'AMATH231 'PMATH347))))

;; Problem 2a --------------------------------------------------------------------------------------

;; (missed-deadline-add selections username)  finds desired courses or returns selections with
;; users at the end showing empty

;; example

(check-expect
 (missed-deadline-add selections "alice1")
 selections)


;; (missed-deadline-add selections username):
;; (cons (list Str (listof Sym)) DesiredCourses) str -> (cons (list Str (listof Sym)) DesiredCourses)
(define (missed-deadline-add selections username)
  (cond
    [(empty? selections) (list (list username empty))]
    [(string=? username (first (first selections))) selections]
    [else (cons (first selections) (missed-deadline-add (rest selections) username))]))

;;tests
(check-expect
 (missed-deadline-add selections "dave4")
 (list
   (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
   (list "alice1" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
   (list "abc123" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
   (list "s576shar" (list 'CLAS104 'LS201 'AMATH231 'PMATH347))
   (list "dave4" empty)))

(check-expect
 (missed-deadline-add selections "s576shar")
 selections)


;; Problem 2b --------------------------------------------------------------------------------------

;; (taking-course? selections username course-code): returns true if user is in a certin course

;;example

(check-expect
 (taking-course? selections "abc123" 'MATH135)
 true)

;; (taking-course? selections username course-code):
;; (list (cons (list Str (listof Sym)) DesiredCourses) str sym -> bool
(define (taking-course? selections username course-code)
  (cond
    [(empty? selections) false]
    [(string=? username (first (first selections)))
     (in-course? (second (first selections)) course-code)]
    [else (taking-course? (rest selections) username course-code)]))



;; tests;;

(check-expect
 (taking-course? selections "mpines" 'CS135)
 true)

(check-expect
 (taking-course? selections "alice1" 'CS135)
 false)

(check-expect
 (taking-course? selections "s576shar" 'PMATH347)
 true)



;;  (in-course? courses course-code) returns true if course-code is in courses
;; example:
(check-expect (in-course? (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100) 'MATH135) true)


;;  (in-course? courses course-code): (listof sym) sym -> bool
(define (in-course? courses course-code)
  (cond [(empty? courses) false]
        [(symbol=? course-code (first courses)) true]
        [else (in-course? (rest courses) course-code)]
  ))


;; Problem 2c --------------------------------------------------------------------------------------


;; (add-course selections username course-code): adds user course selections
;; or returns existing if already in it

;; example

(check-expect
 (add-course selections "abc123" 'TESTCOURSE)
 (list
(list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
(list "alice1" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
(list "abc123" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100 'TESTCOURSE))
(list "s576shar" (list 'CLAS104 'LS201 'AMATH231 'PMATH347))))

;; ((add-course selections username course-code):
;; (list (cons (list Str (listof Sym)) DesiredCourses) str sym 
;; -> (list (cons (list Str (listof Sym)) DesiredCourses)
(define (add-course selections username course-code)
  (cond
    [(taking-course? selections username course-code) selections]
    [(username-exists selections username)
     (update-existing-user selections username course-code)]
    [(empty? selections) (list (list username (list course-code)))]
    [else (cons (first selections) (add-course (rest selections) username course-code))]))


;; tests

(check-expect
 (add-course selections "mpines" 'CS136)
 (list
(list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100 'CS136))
(list "alice1" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
(list "abc123" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
(list "s576shar" (list 'CLAS104 'LS201 'AMATH231 'PMATH347))))

(check-expect
 (add-course selections "alice1" 'ECON206)
 selections)

(check-expect
 (add-course selections "bob123" 'ENGL101)
 (list
(list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
(list "alice1" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
(list "abc123" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
(list "s576shar" (list 'CLAS104 'LS201 'AMATH231 'PMATH347))
(list "bob123" (list 'ENGL101))))





;; (username-exist selections username) returns true if the user is in the selection list
;; example:

(check-expect (username-exists selections "s576shar") true)

;; (username-exists selections username):
;; (list (cons (list Str (listof Sym)) DesiredCourses) str -> bool
(define (username-exists selections username)
  (cond
    [(empty? selections) false]
    [(string=? (first (first selections)) username) true]
    [else (username-exists (rest selections) username)]))





;; (update-existing-user selections username course-code) adds code to user that exists

;; example:

(check-expect
 (update-existing-user selections "alice1" 'CS136)
 (list
   (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
   (list "alice1" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101 'CS136))
   (list "abc123" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
   (list "s576shar" (list 'CLAS104 'LS201 'AMATH231 'PMATH347))))

;; (update-existing-user selections username course-code):
;; (list (cons (list Str (listof Sym)) DesiredCourses) str ->
;;  (list (cons (list Str (listof Sym)) DesiredCourses)
(define (update-existing-user selections username course-code)
  (cond
    [(string=? (first (first selections)) username)
     (cons (list username (add-to-end (second (first selections)) course-code))
           (rest selections))]
    [else (cons (first selections) (update-existing-user (rest selections) username course-code))]))





;; (add-to-end courses course-code) adds course code to end of list

;; example
(check-expect
 (add-to-end (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101) 'CS136)
 (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101 'CS136))

;; (add-to-end courses course-code):
;; (listof Sym) Sym ->
;;   (listof Sym)
(define (add-to-end courses course-code)
  (cond
    [(empty? courses) (list course-code)]
    [else (cons (first courses) (add-to-end (rest courses) course-code))]))


;; Problem 2d --------------------------------------------------------------------------------------



;; (create-classlist selections course-code) returns all students who want a certin course

;; example

(check-expect (create-classlist selections 'FINE100) (list "mpines" "abc123"))

;; (create-classlist selections course-code):
;; (list (cons (list Str (listof Sym)) DesiredCourses) sym -> (listof str)
(define (create-classlist selections course-code)
  (cond
    [(empty? selections) empty]
    [(in-course? (second (first selections)) course-code)
     (cons (first (first selections)) (create-classlist (rest selections) course-code))]
    [else (create-classlist (rest selections) course-code)]
    ))


;; tests

(check-expect (create-classlist selections 'CLAS104) (list "s576shar"))

(check-expect (create-classlist selections 'MATH137) (list "mpines" "abc123"))

(check-expect (create-classlist selections 'NOoneCLASS) empty)





        




