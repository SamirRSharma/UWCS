;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname median) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define (median-of-3 a b c)
  (cond
    [(or (and (<= b a) (<= a c)) (and (<= c a) (<= a b))) a]
    [(or (and (<= a b) (<= b c)) (and (<= c b) (<= b a))) b]
    [(or (and (<= b c) (<= c a)) (and (<= a c) (<= c b))) c]))


   (define (median-of-3-simple a b c)
  (cond
    [(<= b c)
     (cond
       [(<= a b) b]
       [(<= a c) a]
       [else c])]
    [else
     (cond
       [(<= a c) c]
       [(<= a b) a]
       [else b])]))

   (check-expect (median-of-3-simple 1 2 3) 2)
   (check-expect (median-of-3-simple 2 1 3) 2)

   (check-expect (median-of-3-simple 3 2 1) 2)
   (check-expect (median-of-3-simple 5 3 1) 3)
   (check-expect (median-of-3-simple 1 3 5) 3)
   (check-expect (median-of-3-simple 1 100 1000) 100)
   (check-expect (median-of-3-simple 1 1 2) 1)
   (check-expect (median-of-3-simple 1 1 1) 1)
    (check-expect (median-of-3-simple -2 5 3) 3)
