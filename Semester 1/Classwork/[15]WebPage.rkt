; 15.1  Write a procedure to-binary:

; > (to-binary 9)
; 1001

; > (to-binary 23)
; 10111

(define (toBinary n)
  (cond ((< n 2) n)
        (else (word 
               (toBinary (floor (/ n 2)))
               (remainder n 2)))))


; 15.2  A "palindrome" is a sentence that reads the same backward as forward. Write a predicate palindrome? that takes a sentence as argument and decides whether it is a palindrome. For example:

; > (palindrome? '(flee to me remote elf))
; #T

; > (palindrome? '(flee to me remote control))
; #F

; 

(define (se2wd text)
  (cond ((empty? text) "")
        (else (word (first text) (se2wd (bf text))))))

(define (palindrome?-helper wd)
  (cond ((< (count wd) 2) #t)
        ((eq? (first wd) (last wd)) (palindrome?-helper (bf (bl wd))))
        (else #f)))

(define (palindrome? text)
  (palindrome?-helper (se2wd text)))

; 15.3  Write a procedure substrings that takes a word as its argument. It should return a sentence containing all of the substrings of the argument. A substring is a subset whose letters come consecutively in the original word. For example, the word bat is a subset, but not a substring, of brat.