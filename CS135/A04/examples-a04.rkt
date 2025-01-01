;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname examples-a04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Question 2a)

(check-expect (list-has-exactly-4-symbols (cons 'apple
                                                (cons 'banana
                                                      (cons 'cherry
                                                            (cons 'date empty)))))
              true)
(check-expect (list-has-exactly-4-symbols (cons 'apple
                                                (cons "banana"
                                                      (cons 'cherry
                                                            (cons 42
                                                                  (cons 3 empty))))))
                                          false)
(check-expect (list-has-exactly-4-symbols empty)
              false)

;; Question 2b)

(check-expect (add-only-numbers (cons 1 (cons 2 (cons 3 empty)))) 6)
(check-expect (add-only-numbers (cons "hello" (cons 'world (cons 5 empty)))) 5)
(check-expect (add-only-numbers (cons (cons "apple" empty) (cons 'a (cons "b" empty)))) 0)

;; Question 2c)

(check-expect (before-after "cat" "before" "after" (cons "the" (cons "cat" (cons "sat" empty))))
              (cons "the" (cons "before" (cons "cat" (cons "after" (cons "sat" empty))))))
(check-expect (before-after "dog" "woof" "bark" (cons "dog" (cons "runs" empty)))
              (cons "woof" (cons "dog" (cons "bark" (cons "runs" empty)))))
(check-expect (before-after "fish" "swim" "fins" (cons "birds" (cons "fly" empty)))
              (cons "birds" (cons "fly" empty)))

;; Question 2d)

(check-expect (exists? 'apple (cons 'banana (cons 'apple (cons 'cherry empty)))) true)
(check-expect (exists? "hello" (cons 'hi (cons "hello" (cons 'hey empty)))) true)
(check-expect (exists? 42 (cons 1 (cons 2 (cons 3 empty)))) false)

;; Question 2e)

(check-expect (remove-duplicates
               (cons 'testing (cons 9 (cons 'testing (cons 135
               (cons 'banana (cons 135 (cons "MATH" (cons 135
               (cons 'hi empty))))))))))
              (cons 9 (cons 'testing (cons 'banana (cons "MATH" (cons 135 (cons 'hi
            empty)))))))

;; HAPPY 3 HAPPY 2 cs 2 CS 2 5
;; 3 HAPPY cs CS 2 5








;; Question 3a)

(check-expect (sorted? (cons (make-card 3 'Heart)
                             (cons (make-card 5 'Spade) empty)))
              true )
(check-expect (sorted? (cons (make-card 10 'Diamond)
                             (cons (make-card 10 'Club) empty)))
              false)

;; Question 3b)

(check-expect (cheater? (cons (make-card 5 'Heart)
                              (cons (make-card 5 'Heart)
                                    (cons (make-card 7 'Club) empty))))
              true)
(check-expect (cheater? (cons (make-card 9 'Spade)
                              (cons (make-card 9 'Heart)
                                    (cons (make-card 9 'Club) empty))))
              false)

;; Question 3c)

(check-expect (is-straight? (cons (make-card 6 'Club)
                                  (cons (make-card 7 'Diamond)
                                        (cons (make-card 8 'Heart)
                                              (cons (make-card 9 'Spade)
                                                    (cons (make-card 10 'Club) empty))))))
              true)
(check-expect (is-straight? (cons (make-card 2 'Club)
                                  (cons (make-card 3 'Diamond)
                                        (cons (make-card 4 'Heart)
                                              (cons (make-card 5 'Spade)
                                                    (cons (make-card 7 'Club) empty))))))
              false)

;; Question 3d)

(check-expect (is-flush? (cons (make-card 2 'Heart)
                               (cons (make-card 5 'Heart)
                                     (cons (make-card 8 'Heart)
                                           (cons (make-card 9 'Heart)
                                                 (cons (make-card 10 'Heart) empty))))))
              true)
(check-expect (is-flush? (cons (make-card 3 'Club)
                               (cons (make-card 5 'Heart)
                                     (cons (make-card 7 'Spade)
                                           (cons (make-card 9 'Diamond)
                                                 (cons (make-card 10 'Club) empty))))))
              false)

;; Question 3e)

(check-expect (is-full-house? (cons (make-card 6 'Club)
                                    (cons (make-card 6 'Diamond)
                                          (cons (make-card 6 'Heart)
                                                (cons (make-card 7 'Heart)
                                                      (cons (make-card 7 'Spade) empty))))))
              true)
(check-expect (is-full-house? (cons (make-card 2 'Heart)
                                    (cons (make-card 2 'Spade)
                                          (cons (make-card 3 'Club)
                                                (cons (make-card 3 'Diamond)
                                                      (cons (make-card 4 'Heart) empty))))))
              false)

;; Question 3f)

(check-expect (replace-card (make-card 5 'Heart) (make-card 3 'Spade)
                            (cons (make-card 2 'Club)
                                  (cons (make-card 5 'Heart)
                                        (cons (make-card 7 'Diamond)
                                              (cons (make-card 6 'Heart)
                                                    (cons (make-card 9 'Spade) empty))))))
              (cons (make-card 2 'Club)
                    (cons (make-card 3 'Spade)
                          (cons (make-card 7 'Diamond)
                                (cons (make-card 6 'Heart)
                                      (cons (make-card 9 'Spade) empty))))))

(check-expect (replace-card (make-card 3 'Diamond) (make-card 2 'Club)
                            (cons (make-card 3 'Diamond)
                                  (cons (make-card 9 'Heart)
                                        (cons (make-card 4 'Diamond)
                                              (cons (make-card 5 'Spade) empty)))))
              (cons (make-card 2 'Club)
                    (cons (make-card 9 'Heart)
                          (cons (make-card 4 'Diamond)
                                (cons (make-card 5 'Spade) empty)))))

