; 14.3
; (remdup '(ob la di ob la da))  ; remove duplicates
; '(ob la di da)

; It's okay if your procedure returns '(di ob la da) instead, 
; as long as it removes all but one instance of each duplicated word.

(define (remdup str)
    (remdup-helper str '()))

(define (remdup-helper str set)
    (cond ((empty? str) '())
          ((member? (first str) set) (remdup-helper (bf str) set))
          (else (se (first str) (remdup-helper (bf str) (se set (first str)))))))


; ;14.4
; (odds '(i lost my little girl))
; '(i my girl)

(define (odds str)
    (cond ((empty? str) '())
          ((= (count str) 1) (first str))
          (else (se (first str) (odds (bf (bf str)))))))

; ; 14.5 Write a procedure letter-count that 
; takes a sentence as its argument and 
; returns the total number of letters in the sentence:

; (letter-count '(fixing a hole))
; 11

(define (letter-count str)
    (if (empty? str)
        0
        (+ (count (first str)) (letter-count (bf str)))))

; ;14.6 Write member?.

(define (member? target list)
    (cond ((empty? list) #f)
          ((eq? target (first list)) #t)
          (else (member? target (bf list)))))

; ;14.7 Write differences, which 
; takes a sentence of numbers as its argument and 
; returns a sentence containing the differences between adjacent elements.  
; (The length of the returned sentence is one less than that of the argument.)

; (differences '(4 23 9 87 6 12))
; '(19 -14 78 -81 6)

(define (differences str)
    (cond ((= (count str) 2) (se (- (last str) (first str))))
          (else (se (- (first (bf str)) (first str)) (differences (bf str))))))

; ;14.9 Write a procedure called location that 
; takes two arguments, a word and a sentence.  
; It should return a number indicating where in the sentence that word can be found.  
; If the word isn't in the sentence, return #f.  
; If the word appears more than once, return the location of the first appearance.

; (location 'me '(you never give me your money))
; 4

(define (location target str)
    (location-helper target str 1))

(define (location-helper target str idx)
    (cond ((eq? (item idx str) target) idx)
          (else (location-helper target str (+ idx 1)))))

; ;14.10 Write the procedure count-adjacent-duplicates that 
; takes a sentence as an argument and 
; returns the number of words in the sentence that are immediately followed by the same word:

; (count-adjacent-duplicates '(y a b b a d a b b a d o o))
; 3

; (count-adjacent-duplicates '(yeah yeah yeah))
; 2

(define (count-adjacent-duplicates str)
    (cond ((< (count str) 2) 0)
          ((eq? (first str) (first (bf str))) (+ (count-adjacent-duplicates (bf str)) 1))
          (else (count-adjacent-duplicates (bf str)))))

; ;14.11 Write the procedure remove-adjacent-duplicates that
; takes a sentence as argument and 
; returns the same sentence but with any word that's immediately followed by the same word removed:

; (remove-adjacent-duplicates '(y a b b a d a b b a d o o))
; '(y a b a d a b a d o)

; (remove-adjacent-duplicates '(yeah yeah yeah))
; '(yeah)

(define (remove-adjacent-duplicates str)
    (cond ((< (count str) 2) str)
          ((eq? (last str) (last (bl str))) (remove-adjacent-duplicates (bl str)))
          (else (se (remove-adjacent-duplicates (bl str)) (last str)))))

; ;14.15 Write merge, a procedure that takes two sentences of numbers as arguments.  
; Each sentence must consist of numbers in increasing order.  
; merge should return a single sentence containing all of the numbers, in order.  
; (We'll use this in the next chapter as part of a sorting algorithm.)

; (merge '(4 7 18 40 99) '(3 6 9 12 24 36 50))
; '(3 4 6 7 9 12 18 24 36 40 50 99)

(define (merge arr1 arr2)
    (cond ((empty? arr1) arr2)
          ((empty? arr2) arr1)
          ((< (first arr1) (first arr2)) (se (first arr1) (merge (bf arr1) arr2)))
          (else (se (first arr2) (merge arr1 (bf arr2))))))