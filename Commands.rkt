;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Commands) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
cx	check-expect
cw	check-within
ce	check-error
df	define function
d	define
ds	define structure
m	make structure
c	cond with else
if	if statement
con	list in cons notation
li	abbreviated list
dsp	display
t	true
f	false
e	empty
hdr	file header
cmt	comment
l	;;


ll	3-line comment
pa	3-line "Part a" comment
pb	3-line "Part b" comment
pc	3-line "Part c" comment
pd	3-line "Part d" comment
pe	3-line "Part e" comment
pf	3-line "Part f" comment
ppe	Purpose and examples
ctr 	Contract
req	Requires
tst	Tests
lox	list-of-X Template
nelox	ne-List-of-X Template
lo	(listof )
nelo	(ne-listof )
lost	(listof Str)
nelost	(ne-listof Str)
lob	(listof Bool)
nelob	(ne-listof Bool)
loc	(listof Char)
neloc	(ne-listof Char)
losy	(listof Sym)
nelosy	(ne-listof Sym)
lonu	(listof Num)
nelonu	(ne-listof Num)
lona	(listof Nat)
nelona	(ne-listof Nat)

Alt+R	Run the program	Ctrl+R or F5
Ctrl+Shift+F	Indent all lines	Ctrl+I
Ctrl+H	Show Replace	Ctrl+Shift+R
Ctrl+Shift+R	Replace All	None
Ctrl+/	Comment out line with ;	None
Ctrl+Backspace	Delete word left of cursor	Alt+Backspace
Ctrl+Delete	Delete word right of cursor	Alt+Delete
Alt+Up/Down	Switch between Interactions and Definitions panes	Ctrl+F6


;; (listof-X-template lox)
;; Examples:
(check-expect (listof-X-template empty) )
(check-expect (listof-X-template (list )) )

;; listof-X-template: (listof ) -> 
(define (listof-X-template lox)
  (cond [(empty? lox) ]
        [else ( (first lox)
               (listof-X-template (rest lox)))]))

;; Tests


c
