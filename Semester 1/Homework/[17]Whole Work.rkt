(require simply-scheme)

; 17.1  What will Scheme print in response to each of the following expressions? 
; Try to figure it out in your head before you try it on the computer.

; > (car '(Rod Chris Colin Hugh Paul))
'Rod
; > (cadr '(Rod Chris Colin Hugh Paul))
'Chris
; > (cdr '(Rod Chris Colin Hugh Paul))
'(Chris Colin Hugh Paul)
; > (car 'Rod)
"Error: Not a pair"
; > (cons '(Rod Argent) '(Chris White))
'((Rod Argent) Chris White)
; > (append '(Rod Argent) '(Chris White))
'(Rod Argent Chris White)
; > (list '(Rod Argent) '(Chris White))
'((Rod Argent) (Chris White))
; > (caadr '((Rod Argent) (Chris White) (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))
'Chris
; > (assoc 'Colin '((Rod Argent) (Chris White) (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))
'(Colin Blunstone)
; > (assoc 'Argent '((Rod Argent) (Chris White) (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))
#f


; 17.2  For each of the following examples, 
; write a procedure of two arguments that, 
; when applied to the sample arguments, 
; returns the sample result. 
; Your procedures may not include any quoted data.

; > (f1 '(a b c) '(d e f))
; ((B C D))

(define (f1 v1 v2)
    (list (append (cdr v1) (list (cadr v2)))))

; > (f2 '(a b c) '(d e f))
; ((B C) E)

(define (f2 v1 v2)
    (list (cdr v1) (cadr v2)))

; > (f3 '(a b c) '(d e f))
; (A B C A B C)

(define (f3 v1 v2)
    (append v1 v1))

; > (f4 '(a b c) '(d e f))
; ((A D) (B C E F))

(define (f4 v1 v2)
    (list (list (car v1) (car v2)) (append (cdr v1) (cdr v2))))


; 17.4  Describe the result of calling the following procedure with a list as its argument. 
; (See if you can figure it out before you try it.)

(define (mystery lst)
  (mystery-helper lst '()))

(define (mystery-helper lst other)
  (if (null? lst)
      other
      (mystery-helper (cdr lst) (cons (car lst) other))))

"17.4: Function is ... to reverse the list"


; 17.8  Write member.

; Oh the problem is soooo short.....

(define (my-member target list)
    (cond ((null? list) #f)
          ((eq? (car list) target) list)
          (else (my-member target (cdr list)))))

; 17.9  Write list-ref.

(define (my-list-ref list idx)
    (cond ((null? list) (error "list-ref: index too large for list\n"))
          ((= idx 0) (car list))
          (else (my-list-ref (cdr list) (- idx 1)))))


; 17.10  Write length.

(define (my-length list)
    (cond ((null? list) 0)
          (else (+ 1 (my-length (cdr list))))))


; 17.12  Write a procedure called flatten that takes as its argument a list, 
; possibly including sublists, 
; but whose ultimate building blocks are words (not Booleans or procedures). 
; It should return a sentence containing all the words of the list, 
; in the order in which they appear in the original:

; > (flatten '(((a b) c (d e)) (f g) ((((h))) (i j) k)))
; (A B C D E F G H I J K)

(define (flatten lst)
    (cond ((plain-list? lst) lst)
          (else (flatten (reduce append (map (lambda (el) (if (list? el) el (list el))) lst))))))

(define (plain-list? lst)
    (null? (filter list? lst)))


; 17.14  Write a procedure branch that 
; takes as arguments a list of numbers and a nested list structure. 
; It should be the list-of-lists equivalent of item, like this:

; > (branch '(3) '((a b) (c d) (e f) (g h)))
; (E F)

; > (branch '(3 2) '((a b) (c d) (e f) (g h)))
; F

; > (branch '(2 3 1 2) '((a b) ((c d) (e f) ((g h) (i j)) k) (l m)))
; H
; In the last example above, the second element of the list is

; ((C D) (E F) ((G H) (I J)) K)
; The third element of that smaller list is ((G H) (I J)); 
; the first element of that is (G H); and the second element of that is just H.

(define (branch path lst)
    (cond ((null? path) lst)
          (else (branch (cdr path) (list-ref lst (- (car path) 1))))))