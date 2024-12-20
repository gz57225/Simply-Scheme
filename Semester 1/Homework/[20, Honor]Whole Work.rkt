; 20.1  What happens when we evaluate the following expression? 
; What is printed, and what is the return value? 
; Try to figure it out in your head before you try it on the computer.

(define __
(cond ((= 2 3) (show '(lady madonna)) '(i call your name))
      ((< 2 3) (show '(the night before)) '(hello little girl))
      (else '(p.s. i love you)))
)

"RETURN: "
'(hello little girl)
"STDOUT: "
(the night before)
'(hello little girl)


; 20.2  What does newline return in your version of Scheme?
; ------------------------------------------------------
; ------------------------------------------------------
; ------------------------------------------------------

"It said #<void> in the codespace"

; 20.3  Define show in terms of newline and display.
; ------------------------------------------------------
; ------------------------------------------------------
; ------------------------------------------------------

(define (show input)
    (begin
        (display input)
        (newline )))

; 20.4  Write a program that carries on a conversation like the following example. 
; What the user types is in boldface.

; > (converse)
; Hello, I'm the computer.  What's your name? Brian Harvey
; Hi, Brian.  How are you? I'm fine.
; Glad to hear it.

(define (siri)
    (read-line )
    (begin
        "AI, stands for Awkward Interactions, I know it won't display, But I just want to say, it's so awkward!"
        (show "Hello, I'm the computer.  What's your name?")
        ; I don't know why 'read' only accept exactly one word or list in my codespace
        (display (word "Hi, " (car (read-line )) ".  How are you?"))
        (newline )
        (read-line)
        (show "Glad to hear it.")))

; 20.5  Our name-table procedure 
; uses a fixed width for the column containing the last names of the people in the argument list. 
; Suppose that instead of liking British-invasion music you are into late romantic Russian composers:

; > (name-table '((piotr tchaikovsky) (nicolay rimsky-korsakov) (sergei rachmaninov) (modest musorgsky)))

(define (name-table names)
  (if (null? names)
      (display done)
      (begin (display (align (cadar names) 30))
	     (show (caar names))
	     (name-table (cdr names)))))

;      _____   _____   ____________  _____         _____         ________________ 
;      |   |   |   |   |   _______|  |   |         |   |         |   ________   |                                            
;      |   |   |   |   |   |         |   |         |   |         |   |      |   |                                        
;      |   |___|   |   |   |         |   |         |   |         |   |      |   |                           
;      |           |   |   |______   |   |         |   |         |   |      |   |                           
;      |   _____   |   |   _______|  |   |         |   |         |   |      |   |                    
;      |   |   |   |   |   |         |   |         |   |         |   |      |   |                     
;      |   |   |   |   |   _______|  |   |_______  |   |_______  |   |______|   |                       
;      |___|   |___|   |__________|  |__________|  |__________|  |______________|          

; TAKE A LOT OF TIME TO FIX!

; Tik Tac Toe Game ---------------------------------------------------------
; Here We Go ---------------------------------------------------------
; Alright ---------------------------------------------------------

(define (ttt-ai game-map letter)
  (find-pos-of '_ game-map))

(define (find-pos-of letter word)
  (if (equal? letter (first word))
      1
      (+ 1 (find-pos-of letter (bf word)))))

(define (play-ttt put-strategy-of-x put-strategy-of-o)
  (let ((res (play-ttt-helper put-strategy-of-x put-strategy-of-o '_________ 'x)))
    (begin
      (show (car res))
      (show "THE FINAL GAME MAP IS ...")
      (print-game-map (cadr res)))))

(define (play-ttt-helper put-strategy-of-x put-strategy-of-o game-map cur-player)
  (cond ((already-won? game-map (opponent cur-player))
	          (list (list (opponent cur-player) 'wins!) game-map))
	      ((tie-game? game-map)
            (list '(tie game) game-map))
	      (else 
            (let ((idx-going-to-be-put 
                          (if (equal? cur-player 'x) (put-strategy-of-x game-map 'x) (put-strategy-of-o game-map 'o))))
		            (play-ttt-helper 
                   put-strategy-of-x
                   put-strategy-of-o
				          (add-move idx-going-to-be-put cur-player game-map)
				          (opponent cur-player))))))

(define (already-won? game-map player)
  (ormap (lambda (pattern)
           (andmap (lambda (pos) (eq? (get-between game-map pos pos) player)) pattern))
         (list (list 1 2 3) (list 4 5 6) (list 7 8 9)
               (list 1 4 7) (list 2 5 8) (list 3 6 9)
               (list 1 5 9) (list 3 5 7))))
          
(define (tie-game? game-map)
  (and 
    (not 
      (ormap 
        (lambda (pos) (eq? (get-between game-map pos pos) '_))
        (list 1 2 3 4 5 6 7 8 9)))
    (not 
      (or 
        (already-won? game-map 'x) 
        (already-won? game-map 'o)))))

(define (opponent player)
  (if (eq? player 'x) 'o 'x))

(define (add-move idx cur-player game-map)
  (if (= idx 1)
      (word cur-player (bf game-map))
      (word (first game-map)
	    (add-move (- idx 1) cur-player (bf game-map)))))

(define (ask-user game-map cur-player)
  (print-game-map game-map)
  (display cur-player)
  (display "'s move: ")
  (read))

(define (print-game-map game-map)
  (print-row (get-between game-map 1 3))
  (show "-+-+-")
  (print-row (get-between game-map 4 6))
  (show "-+-+-")
  (print-row (get-between game-map 7 9))
  (newline))

(define (print-row row)
  (ttt-display-per-block (first row))
  (display "|")
  (ttt-display-per-block (first (bf row)))
  (display "|")
  (ttt-display-per-block (last row))
  (newline))

(define (ttt-display-per-block letter)
  (if (not (equal? letter '_))
      (display letter)
      (display " ")))

(define (get-between wd start end)
  ((repeated bf (- start 1))
   ((repeated bl (- (count wd) end))
    wd)))

; Here's how it works:
; > (print-game-map '_x_oo__xx)
;  |X| 
; -+-+-
; O|O| 
; -+-+-
;  |X|X

(define m1 'xoooxxxoo)
'(
xoo
oxx
xoo
)
(define m2 'oxoxooxoo)
'(
oxo
xoo
xoo
)
(define m3 '_xox__x_o)
'(
_xo
x__
x_o
)



"MORE ROBUST ONE"

(define (ask-user game-map letter)
  (print-game-map game-map)
  (display letter)
  (display "'s move: ")
  (ask-again 0 game-map))

(define (ask-again time game-map)
    (begin
        (if (not (eq? time 0)) (display "Exception : Wrong Input! Requirements: Integer[1,9] & Not filled: ") null)
        (let ((input (read)))
            (if (and 
                    (word? input) 
                    (member? input '(1 2 3 4 5 6 7 8 9))
                    (eq? '_ (get-between game-map input input)))
              input 
              (ask-again 1 game-map)))))


(define (game)
  (begin
    (display "Would you like to play as x or o (x is first hand): ")
    (game-helper)))

(define (game-helper)
  (begin
    (let ((input (read )))
      (cond ((eq? input 'x) (play-ttt ask-user ttt-ai))
            ((eq? input 'o) (play-ttt ttt-ai ask-user))
            (else (begin (display "Wrong Choice!, Please input again: ") (game-helper)))))))

(define (games)
  (begin
    (game)
    (newline )
    (show "What about? One more play (Press any key to continue except n)")
    (cond ((eq? (read ) 'n) (error "ERROR: BECAUSE YOU DO NOT WANNA PLAY MORE!"))
          (else (games)))))