;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname roadtrip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define (city-value city)
  (cond
    [(symbol=? city 'StJohns) 1]
    [(symbol=? city 'Charlottetown) 2]
    [(symbol=? city 'Halifax) 3]
    [(symbol=? city 'Fredericton) 4]
    [(symbol=? city 'QuebecCity) 5]
    [(symbol=? city 'Toronto) 6]
    [(symbol=? city 'Waterloo) 7]
    [(symbol=? city 'SaultSteMarie) 8]
    [(symbol=? city 'ThunderBay) 9]
    [(symbol=? city 'Winnipeg) 10]
    [(symbol=? city 'Regina) 11]
    [(symbol=? city 'Calgary) 12]
    [(symbol=? city 'Vancouver) 13]))

(define (helper current next fatigue-level)
  (cond
    ;; same city
    [(and (= current next) (symbol=? fatigue-level 'exhausted)) 'ready]
    [(and (= current next) (symbol=? fatigue-level 'ready)) 'rested]
    [(= current next) 'rested]
    ;; one city distance travel
    [(and (= (- next current) 1) (symbol=? fatigue-level 'rested)) 'ready]
    [(and (= (- next current) 1) (symbol=? fatigue-level 'ready)) 'ready]
    ;; two cities distance travel
    [(and (= (- next current) 2) (symbol=? fatigue-level 'rested)) 'exhausted]
    ;; invalid travel
    [else 'invalid]))

; Main function
(define (check-plan city1 city2 city3 city4)
  (cond[(symbol=? (helper
                   (city-value city1) (city-value city2) 'rested) 'invalid) 'invalid] ; leg 1 invalid

       [(symbol=? (helper (city-value city2) (city-value city3) 
                          (helper (city-value city1)
                                  (city-value city2)
                                  'rested)) 'invalid) 'invalid] ; leg 2 invalid

       [(symbol=? (helper
                   (city-value city3) (city-value city4)
                   (helper (city-value city2) (city-value city3)
                           (helper (city-value city1) (city-value city2) 'rested)))
                  'invalid) 'invalid] ; leg 3 invalid

 ;; calculuate final rested value
       [else (helper (city-value city3) (city-value city4) 
                     (helper (city-value city2) (city-value city3) 
                             (helper (city-value city1) (city-value city2) 'rested)))]))


; Test cases
(check-expect (check-plan 'Halifax 'Fredericton 'Halifax 'Fredericton) 'invalid)
(check-expect (check-plan 'Waterloo 'Waterloo 'Waterloo 'Waterloo) 'rested)
(check-expect (check-plan 'Halifax 'QuebecCity 'QuebecCity 'Toronto) 'ready)
(check-expect (check-plan 'Halifax 'QuebecCity 'QuebecCity 'Waterloo) 'invalid)
(check-expect (check-plan 'Calgary 'Calgary 'Vancouver 'Vancouver) 'rested)
(check-expect (check-plan 'Charlottetown 'Charlottetown 'Charlottetown 'Fredericton) 'exhausted)
(check-expect (check-plan 'SaultSteMarie 'ThunderBay 'Winnipeg 'Regina) 'ready)
(check-expect (check-plan 'Halifax 'SaultSteMarie 'Winnipeg 'Vancouver) 'invalid)
(check-expect (check-plan 'Halifax 'Halifax 'Halifax 'Toronto) 'invalid)
(check-expect (check-plan 'Halifax 'Fredericton 'QuebecCity 'Toronto) 'ready)
(check-expect (check-plan 'StJohns 'StJohns 'Halifax 'QuebecCity) 'invalid)
(check-expect (check-plan 'Charlottetown 'QuebecCity 'Vancouver 'Vancouver) 'invalid)
(check-expect (check-plan 'Calgary 'Calgary 'Calgary 'Calgary) 'rested)  ; CALGARY WHOOOOOOOO
(check-expect (check-plan 'SaultSteMarie 'ThunderBay 'Winnipeg 'Regina) 'ready)
