;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname examples-a06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; examples


;; 1a

(check-expect (my-list-ref (list "a" "b" "c" "d") 1) "b")
(check-expect (my-list-ref (list 3 16 24) 2) 24)
(check-expect (my-list-ref (list 3) 8) false)

;; 1b

(check-expect (zip (list "a" "b" "c" "d") (list 1 2 3 4))
(list (list "a" 1) (list "b" 2) (list "c"3) (list "d"
4)))

(check-expect (zip empty empty) empty)

;; 1c

(check-expect
(list-xor (list 2 4 6) (list 1 3 5)) (list 1 2 4 5)


;; 2a

;; n/a

;; 2b

;;(check-expect (matrix-item M 1 0) 5)

;;(check-expect (matrix-item M 1 3) 8)

;; 2c

;;(check-expect (matrix-col M 3) 4 8)
;;(check-expect (matrix-col M 0) 1 5)

;; 3 define

;;(define action1 (make-action 3 "Prepared assignment question"))
;;(define action2 (make-action -7 "Questions are too hard"))
;;(define actlst (list (list "Samir" (list action1 action2))))
;;(define action3 (make-action 42 "Told a good joke about recursion."))
;;(define newactlst (list (list "Samir" (list action3 action1
;;action2))))
;;(define wish1 (make-wish 32 "Passing CS 135"))
;;(define wish2 (make-wish 99 "Passing First Year"))
;;(define chldlst (list (list "Samir" (list wish1 wish2))))

 ;; 3a

(check-expect (extreme-actions "Samir" actlst) (list "Questions are too hard" "Prepared assignment question"))

;; 3b

(check-expect (merge-actions actlst (list (list "Samir" action3))) newactlst)

;; 3c

(check-expect (assign-gifts actlst chldlst)
(list (list "Samir" (list "coal"))))
(check-expect (assign-gifts newactlst chldlst)
(list (list "Samir" (list "Passing CS 135"))))