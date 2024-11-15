(require simply-scheme)


(define (every procedure list)
   (cond ((empty? list) '())
         (else (se (procedure (first list)) (every procedure (bf list))))))

(define (keep cond? list)
   (if (empty? list)
       list
       (if (cond? (first list))
           ((if (word? list) word se) (first list) (keep cond? (bf list)))
           (keep cond? (bf list)))))

; Accumulate Start
(define (accumulate func list)
   (acc-helper func (if (word? list) (to-sentence list) list)))


(define (to-sentence list)
   (if (empty? list)
       '()
       (se (first list) (to-sentence (bf list)))))


(define (acc-helper func list)
   (cond ((= (count list) 1) (first list))
         ((> (count list) 1) (acc-helper func (se (bl (bl list)) (func (last (bl list)) (last list)))))))
; Accumulate End
