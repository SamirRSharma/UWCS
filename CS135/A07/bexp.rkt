;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bexp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;    Samir Sharma (21116578)
;;    CS 135 Fall 2024
;;    A07 , Question 2
;; ***************************************************
;;

;; was given an extention until friday 9pm, talk to karen anderson

;;
;; Part a
;;


;; A Boolean is one of:
;; - 0
;; - 1

;; An OpNode is a (list Sym (listof BExp))
;; Requires: Sym is one of 'AND, 'OR, 'XOR
;; and (listof BExp) has at least one element

;; A BExp is one of:
;; - Boolean
;; - OpNode

;; bexp-template: BExp -> Any
(define (bexp-template bexp)
  (cond
    [(number? bexp) ...]
    [(list? bexp) (opnode-template bexp)]
    [else (error "Invalid BExp")]))

;; opnode-template: OpNode -> Any
(define (opnode-template opnode)
  (... (first opnode)
       (args-template (second opnode))))

;; args-template: (listof BExp) -> Any
(define (args-template args)
  (cond
    [(empty? args) ...]
    [else (... (bexp-template (first args))
               (args-template (rest args)))]))

;;
;; Part b
;;


;; (eval bexp) evaluates the Boolean Expression bexp and produces its value 
;; examples:
(check-expect (eval (list 'AND (list 0 1 1))) 0)
(check-expect (eval (list 'OR (list 0 1 1))) 1)
(check-expect (eval (list 'XOR (list 0 1 1))) 0)
(check-expect (eval (list 'AND (list 1 (list 'XOR (list 0 1 (list 'OR (list 0 0 0)))) 1))) 1)

;; eval: BExp -> Boolean
(define (eval bexp)
  (cond
    [(number? bexp)
     (valid-boolean-value bexp)]
    [(list? bexp)
     (eval-opnode bexp)]))

;; Tests:
(check-expect (eval 1) 1)
(check-expect (eval 0) 0)

;; (valid-boolean-value n) checks if n is 0 or 1
;; examples:
(check-expect (valid-boolean-value 1) 1)

;; valid-boolean-value: Num -> Boolean
(define (valid-boolean-value n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (error "Invalid Boolean value")]))

;; (eval-opnode opnode) produces boolean of first opnode
;; examples:
(check-expect (eval-opnode (list 'AND (list 0 1 1))) 0)
;; eval-opnode: OpNode -> Boolean
(define (eval-opnode opnode)
  (cond
    [(symbol=? (first opnode) 'AND) (eval-and (second opnode))]
    [(symbol=? (first opnode) 'OR) (eval-or (second opnode))]
    [(symbol=? (first opnode) 'XOR) (eval-xor (second opnode))]))

;; (eval-and args) evaluates argument for AND
;; examples:
(check-expect (eval-and (list 1 1 1)) 1)

;; eval-and: (listof BExp) -> Boolean
(define (eval-and args)
  (cond
    [(empty? args) 1]
    [(= (eval (first args)) 0) 0]
    [else (eval-and (rest args))]))

;; (eval-or args) evaluates args for the OR operation
;; examples:
(check-expect (eval-or (list 0 1 0)) 1)

;; eval-or: (listof BExp) -> Boolean
(define (eval-or args)
  (cond
    [(empty? args) 0]
    [(= (eval (first args)) 1) 1]
    [else (eval-or (rest args))]))

;; (eval-xor args) evaluates args for the XOR operation
;; examples:
(check-expect (eval-xor (list 1 0 1)) 0)
;; eval-xor: (listof BExp) -> Boolean
(define (eval-xor args)
  (eval-xor-accumulate args 0))

;; (eval-xor-accumulate args acc) accumulates the XOR
;; examples:
(check-expect (eval-xor-accumulate (list 1 0 1) 0) 0)
;; eval-xor-helper: (listof BExp) Boolean -> Boolean
(define (eval-xor-accumulate args acc)
  (cond
    [(empty? args) acc]
    [else (eval-xor-accumulate (rest args)
            (xor-op acc (eval (first args))))]))

;; (xor-op a b) computes the XOR of two values
;; examples:
(check-expect (xor-op 0 0) 0)
(check-expect (xor-op 0 1) 1)
;; xor-op: Boolean Boolean -> Boolean
(define (xor-op a b)
  (cond
    [(= a b) 0]
    [else 1]))

;;
;; Part c
;;


;; A BIDExp is one of:
;; - Boolean, Sym, OpNode

;; (bidexp->string bexp) produces the string of BIDExp
;; Examples:
(check-expect (bidexp->string (list 'AND (list 0 1 1))) "(f*t*t)")

;; bidexp->string: BIDExp -> Str
(define (bidexp->string bexp)
  (cond
    [(number? bexp)
     (boolean-to-char bexp)]
    [(symbol? bexp)
     (string-append "'" (symbol->string bexp))]
    [(list? bexp)
     (string-append "("
                    (bidexp->string-args (second bexp) (operator-char (first bexp)))
                    ")")]
    [else (error "Invalid expression")]))

;; tests
(check-expect (bidexp->string (list 'OR (list 0 1 1))) "(f+t+t)")
(check-expect (bidexp->string (list 'XOR (list 0 1 1))) "(f.t.t)")

;; (boolean-to-char b) converts Boolean b to "t" or "f
;; example
(check-expect (boolean-to-char 1) "t")

;; boolean-to-char: Num -> Str
(define (boolean-to-char b)
  (cond
    [(= b 1) "t"]
    [(= b 0) "f"]))

;; (operator-char op) returns the string of the operator
;; example
(check-expect (operator-char 'AND) "*")

;; operator-char: Sym -> Str
(define (operator-char op)
  (cond
    [(symbol=? op 'AND) "*"]
    [(symbol=? op 'OR) "+"]
    [(symbol=? op 'XOR) "."]))

;; (bidexp->string-args args opstr) links string version of args
;; examples

(check-expect (bidexp->string-args (list 1 0 1) "+") "t+f+t")
;; bidexp->string-args: (listof BIDExp) Str -> Str
(define (bidexp->string-args args opstr)
  (cond
    [(empty? args) ""]
    [(empty? (rest args))
     (bidexp->string (first args))]
    [else
     (string-append (bidexp->string (first args))
                    opstr
                    (bidexp->string-args (rest args) opstr))]))

;;
;; Part d
;;

;; (eval-id bexp table) evaluates the BIDExp bexp 
;; Examples
(define identifier-table (list (list 'x 1) (list 'y 0)))
(check-expect (eval-id (list 'AND (list 0 'x 1)) identifier-table) 0)

;; eval-id: BIDExp (listof (list Sym Boolean)) -> Boolean
(define (eval-id bexp table)
  (cond
    [(number? bexp)
     (valid-boolean-value bexp)]
    [(symbol? bexp)
     (lookup bexp table)]
    [(list? bexp)
     (eval-opnode-id bexp table)]))

;; tests
(check-expect (eval-id (list 'OR (list 'x 'y 1)) identifier-table) 1)
(check-expect (eval-id (list 'XOR (list 0 'y 1)) identifier-table) 1)

;; (eval-opnode-id opnode table) evaluates the OpNode using table
;; example
(check-expect (eval-opnode-id (list 'AND (list 'x 'y)) identifier-table) 0)
;; eval-opnode-id: OpNode (listof (list Sym Boolean)) -> Boolean
(define (eval-opnode-id opnode table)
  (cond
    [(symbol=? (first opnode) 'AND) (eval-and-id (second opnode) table)]
    [(symbol=? (first opnode) 'OR) (eval-or-id (second opnode) table)]
    [(symbol=? (first opnode) 'XOR) (eval-xor-id (second opnode) table)]))

;; (eval-and-id args table) evaluates args for AND using table
;; example
(check-expect (eval-and-id (list 'x 1) identifier-table) 1)
;; eval-and-id: (listof BIDExp) (listof (list Sym Boolean)) -> Boolean
(define (eval-and-id args table)
  (cond
    [(empty? args) 1]
    [(= (eval-id (first args) table) 0) 0]
    [else (eval-and-id (rest args) table)]))

;; (eval-or-id args table) evaluates args for OR using table
;; example
(check-expect (eval-or-id (list 0 'y) identifier-table) 0)
;; eval-or-id: (listof BIDExp) (listof (list Sym Boolean)) -> Boolean
(define (eval-or-id args table)
  (cond
    [(empty? args) 0]
    [(= (eval-id (first args) table) 1) 1]
    [else (eval-or-id (rest args) table)]))

;; (eval-xor-id args table) evaluates args for XOR using table
;; example
(check-expect (eval-xor-id (list 'x 'x) identifier-table) 0)
;; eval-xor-id: (listof BIDExp) (listof (list Sym Boolean)) -> Boolean
(define (eval-xor-id args table)
  (eval-xor-id-accumulate args 0 table))

;; (eval-xor-id-accumulate args acc table) accumulates the XOR using table
;; Examples:
(check-expect (eval-xor-id-accumulate (list 'x 'y) 0 identifier-table) 1)
;; eval-xor-id-accumulate: (listof BIDExp) Boolean (listof (list Sym Boolean)) -> Boolean
(define (eval-xor-id-accumulate args acc table)
  (cond
    [(empty? args) acc]
    [else (eval-xor-id-accumulate (rest args)
            (xor-op acc (eval-id (first args) table))
            table)]))

;; (lookup sym table) looks up sym in table and returns its value
;; Examples:
(check-expect (lookup 'x identifier-table) 1)
;; lookup: Sym (listof (list Sym Boolean)) -> Boolean
(define (lookup sym table)
  (cond
    [(empty? table) (error "Identifier not found")]
    [(symbol=? sym (first (first table))) (second (first table))]
    [else (lookup sym (rest table))]))