;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define (can-donate-to/cond? donator donatee)
  (cond [ (symbol=? donator 'O-) true]
        [ (symbol=? donatee 'AB+) true]
        [ (symbol=? donator donatee) true]
        [(symbol=? donator 'O+)
         (cond [(symbol=? donatee 'A+) true]
               [(symbol=? donatee 'B+) true]
               [else false])]
         [(symbol=? donator 'A-)
         (cond [(symbol=? donatee 'A+) true]
               [(symbol=? donatee 'AB-) true]
               [else false])]
         [(symbol=? donator 'B-)
         (cond [(symbol=? donatee 'B+) true]
               [(symbol=? donatee 'AB-) true]
               [else false])]  
        [ else false]))


(check-expect (can-donate-to/cond? 'O- 'O-) true)
(check-expect (can-donate-to/cond? 'O- 'AB-) true)
(check-expect (can-donate-to/cond? 'O+ 'O-) false)
(check-expect (can-donate-to/cond? 'O+ 'O+) true)
(check-expect (can-donate-to/cond? 'B+ 'O-) false)
(check-expect (can-donate-to/cond? 'A+ 'AB+) true)
(check-expect (can-donate-to/cond? 'B+ 'B+) true)
(check-expect (can-donate-to/cond? 'AB+ 'O-) false)
(check-expect (can-donate-to/cond? 'B- 'B+) true)


(define (can-donate-to/bool? donator donatee)
  (or (symbol=? donator 'O-) (symbol=? donatee 'AB+) (symbol=? donator donatee)
   (and (symbol=? donator 'O+) (or (symbol=? donatee 'A+) (symbol=? donatee 'B+)))
   (and (symbol=? donator 'A-) (or (symbol=? donatee 'A+) (symbol=? donatee 'AB-)))
   (and (symbol=? donator 'B-) (or (symbol=? donatee 'B+) (symbol=? donatee 'AB-)))))


(check-expect (can-donate-to/bool? 'O- 'O-) true)
(check-expect (can-donate-to/bool? 'O- 'AB-) true)
(check-expect (can-donate-to/bool? 'O+ 'O-) false)
(check-expect (can-donate-to/bool? 'O+ 'O+) true)
(check-expect (can-donate-to/bool? 'B+ 'O-) false)
(check-expect (can-donate-to/bool? 'A+ 'AB+) true)
(check-expect (can-donate-to/bool? 'B+ 'B+) true)
(check-expect (can-donate-to/bool? 'AB+ 'O-) false)
(check-expect (can-donate-to/bool? 'B- 'B+) true)
