(define debug #f)
; @Usage: When debug enabled, Tell me what happened and what's the value of the variable.
(define (logger message i)   
  (if debug (begin (display "[MESSAGE ") (display message) (display "] ") (display i) (show " [END]")) null))

(define colors (list 'RED 'ORANGE 'YELLOW 'GREEN 'BLUE 'PURPLE 'INDIGO 'VIOLET 'PINK 'WHITE))

; @Return answer written in words
; @Example return (RED RED RED YELLOW PINK)
(define (generate-answer num-pegs num-color)
  (cond ((= num-pegs 0) null)
        (else (cons (list-ref colors (random 0 num-color)) (generate-answer (- num-pegs 1) num-color)))))

; @Return how many pegs are in the correct position.
; @Example given (RED YELLOW GREEN) (RED GREEN YELLOW) return 1.
(define (get-correct guess answer) (begin
  (cond ((null? guess) 0)
        ((equal? (car guess) (car answer)) (+ 1 (get-correct (cdr guess) (cdr answer))))
        (else (get-correct (cdr guess) (cdr answer))))))

; @Return how many pegs guessed are in the solution (whatever in the right position or not.)
; @Example given (RED YELLOW GREEN) (RED GREEN YELLOW) return 3.
(define (get-incorrect guess answer) (begin
  (cond ((null? guess) 0)
        ((member? (car guess) answer) (+ 1 (get-incorrect (cdr guess) (remove-first-x answer (car guess)))))
        (else (get-incorrect (cdr guess) answer)))))

; @Return the list without the first 'x'
(define (remove-first-x lst x)
  (cond ((null? lst) null)
        ((equal? (car lst) x) (cdr lst))
        (else (cons (car lst) (remove-first-x (cdr lst) x)))))

(define (display-feedback correct incorrect) (begin
  (newline)
  (display "CORRECT: ")
  (show correct)
  (display "INCORRECT: ")
  (show incorrect)))

; anti-user-friend procedure:
; You need to tell him the # of pegs & colors you want.
(define (play-mastermind num-pegs num-color)
  (let ((answer (generate-answer num-pegs num-color))) (begin ; Every time We Play We generate an answer.
    (logger "ANSWER" answer)
    (read-line) ; Necessary , Because we need it to "reflush" the input, the "return key"
    (guess-loop answer (if debug 114514 10)))))

(define (guess-loop answer rest-time) (begin
    (display "Enter your guess: ")
    (let ((guess (read-line))) (begin
      (logger "GUESS" guess)
      (if (equal? guess answer)
          (show "YOU WIN!")
          (begin
            (display-feedback
              (get-correct guess answer)
              (- (get-incorrect guess answer) (get-correct guess answer)))  ; According to the description of "get-incorrect"!
            (if (> rest-time 0)          ; The end of the game
                (guess-loop answer (- rest-time 1))
                (show "YOU LOSE!"))))))))

; User-friendly procedure.
(define (game) (begin
  (show "Welcome to Mastermind!")
  (display "Choose the number of pegs (1-8): ")
  (start-game-read-pegs-number)))

; @Usage Trying to get how many pegs does user want, then invoke "start-game-read-color-number"
(define (start-game-read-pegs-number) (begin
  (let ((num-pegs (read )))
    (if (and (word? num-pegs) (member? num-pegs '(1 2 3 4 5 6 7 8)))
        (begin
          (display "Choose the number of color (1-10): ")
          (start-game-read-color-number num-pegs))
        (begin
          (display "Exception : Wrong Input! Requirements: Integer[1,8]: ")
          (start-game-read-pegs-number))))))

; @Usage Trying to get how many colors does user want, then invoke "play-mastermind"
(define (start-game-read-color-number num-pegs) (begin
  (let ((num-color (read )))
    (if (and (word? num-color) (member? num-color '(1 2 3 4 5 6 7 8 9 10)))
        (play-mastermind num-pegs num-color)
        (begin
          (display "Exception : Wrong Input! Requirements: Integer[1,10]: ")
          (start-game-read-color-number num-pegs))))))