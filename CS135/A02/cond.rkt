;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(check-expect (q3a-simplified 2 true) 3)
(check-expect (q3a-simplified 0 true) 1)
(check-expect (q3a-simplified -1 true) -2)
(check-expect (q3a-simplified 2 false) 0)
(check-expect (q3a-simplified 0 false) 0)
(check-expect (q3a-simplified -1 false) 0)



(define (q3a n a?)
  (cond [a? (cond [(>= n 0) (+ n 1)]
                  [else (- n 1)])]
        [else 0]))

(define (q3a-simplified n a?)
  (cond [(and a? (>= n 0)) (+ n 1)]
        [a? (- n 1)]
        [else 0]))

(define (q3b a? b? c?)
  (cond
    [a? (cond [b? 'elm]
              [(not c?) 'birch]
              [else 'cedar])]
    [else (cond           
              [b? 'pine]
              [(not c?) 'birch]
              [else 'cherry])]))

(define (q3b-simplified a? b? c?)
  (cond
    [(and a? b?) 'elm]
    [(and a? (not c?)) 'birch]
    [a? 'cedar]
    [b? 'pine]
    [(not c?) 'birch]
    [else 'cherry]))

(define (q3c a? b? c?)
(cond
[(cond [c? b?]
[else (not a?)]) (cond
[b? 'spruce]
[c? 'fir]
[else'larch])]
[else (cond [a? 'hazel]
[else'hickory])]))


(define (q3c-simplified a? b? c?)
  (cond
    [(and b? (or c? (not a?))) 'spruce]
    [(and (not a?) (not b?) (not c?)) 'larch]
    [a? 'hazel]
    [else 'hickory]))



