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





