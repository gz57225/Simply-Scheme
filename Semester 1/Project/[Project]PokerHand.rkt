; https://people.eecs.berkeley.edu/~bh/ssch15/poker.html
; FIX Later: 

; > (poker-value '(h4 s4 c6 s6 c4))
; (FULL HOUSE - FOURS OVER SIXES)

; > (poker-value '(h7 s3 c5 c4 d6))
; (SEVEN-HIGH STRAIGHT)

; > (poker-value '(dq d10 dj da dk))
; (ROYAL FLUSH - DIAMONDS)

; > (poker-value '(da d6 d3 c9 h6))
; (PAIR OF SIXES)

(require simply-scheme)

; Some useful definitions relating to poker hands
(define ranks '(A K Q J 10 9 8 7 6 5 4 3 2 A))
(define suits '(S H D C))
(define (numeric-rank card)
  (let ((r (rank card)))
    (cond ((equal? r 'a) 14)
          ((equal? r 'k) 13)
          ((equal? r 'q) 12)
          ((equal? r 'j) 11)
          (else r))))

; Constructor and selectors for cards
(define (make-card suit rank)  
  (word suit rank))            
(define (rank card) (bf card))
(define (suit card) (first card))

; Some previous problem solutions that are kind of nice to have
; for solving some of the poker hand problems
(define (numeric-differences sent)
  (cond ((< (count sent) 2) '())
        (else (se (- (numeric-rank (item 2 sent)) (numeric-rank (item 1 sent)))
                  (numeric-differences (bf sent))))))

(define (numeric-consec sent)
  (numeric-consec-helper (numeric-rank (first sent)) (every numeric-rank sent) 0 '()))
(define (numeric-consec-helper datum sent current array)
  (cond ((empty? sent) (se array current))
        ((equal? (first sent) datum)
         (numeric-consec-helper datum (bf sent) (+ current 1) array))
        (else (numeric-consec-helper (first sent) (bf sent) 1 (se array current)))))

; Bubble sort program to put the cards in descending order
(define (sort hand)
  ((repeated sort-once (- (count hand) 1)) hand))
(define (sort-once sent)
  (cond ((empty? sent) sent)
        ((eq? (count sent) 1) sent)
        ((> (numeric-rank (first sent))
            (numeric-rank (second sent)))
         (se (first sent) (sort-once (bf sent))))
        (else (se (second sent)
                  (sort-once (se (first sent) (bf (bf sent))))))))

; The main program.  NOTE I have not put in the specifics such as
; which suit made a flush or the rank of a pair; I leave that to 
; the reader to complete.
;
; Note also the decision to sort the hand immediately in the poker-value
; function.  I'm cheating a little bit in that this sort permits me to
; avoid sorting in the predicate functions themselves.
(define (poker-value hand)
  (let ((sortedhand (sort hand)))
    (cond ((royal-flush? sortedhand) 'ROYAL)
          ((straight-flush? sortedhand) 'STRAIGHT-FLUSH)
          ((full-house? sortedhand) 'FULL-HOUSE)
          ((quads? sortedhand) 'QUADS)
          ((flush? sortedhand) 'FLUSH)
          ((straight? sortedhand) 'STRAIGHT)
          ((trips? sortedhand) 'TRIPS)
          ((two-pair? sortedhand) 'TWO-PAIR)
          ((one-pair? sortedhand) 'PAIR)
          (else 'GARBAGE))))

; Some hands for testing
;
; Use: (function (sort hand)) where hand is one of the below
(define roy '(s10 sk sq sa sj))  
; royal flush
(define stf '(s10 s8 s9 sj sq))  
; straight flush
(define qua '(s5 h10 h5 c5 d5)) 
 ; quads (four of a kind)
(define fho '(s10 d7 h7 c10 s7)) 
; full house (three of one kind, two of another)
(define flu '(sa s10 s7 sk s2))  
; flush
(define str '(d8 dj sq h10 d9))  
; straight
(define tri '(d5 s8 sa s5 c5))   
; trips
(define 2pr '(d7 s9 c7 sa h9))   
; two pair
(define 1pr '(d7 s9 c10 sa h9))  
; one pair
(define gbg '(d10 s9 c2 sa h5))  
; garbage

(define (same-suit? hand)
  (let ((the-suit (suit (first hand))))
    (eq? (accumulate + (every (lambda (x) (if (eq? (suit x) the-suit) 0 1)) hand)) 0)))

(define (se-eq? s1 s2)
  (if (= (count s1) (count s2)) (se-eq?-helper s1 s2) #f))

(define (se-eq?-helper s1 s2)
  (cond ((empty? s1) #t)
        ((eq? (first s1) (first s2)) (se-eq?-helper (bf s1) (bf s2)))
        (else #f)))

; (define (poker-value hand)
;   (let ((sortedhand (sort hand)))
;     (cond ((royal-flush? sortedhand) 'ROYAL)

(define (royal-flush? sortedhand)
  (and (same-suit? sortedhand) (se-eq? (every numeric-rank sortedhand) '(14 13 12 11 10))))

;           ((straight-flush? sortedhand) 'STRAIGHT-FLUSH)

(define (straight-flush? sortedhand)
  (and (same-suit? sortedhand) (se-eq? (numeric-differences sortedhand) '(-1 -1 -1 -1))))

;           ((full-house? sortedhand) 'FULL-HOUSE)

(define (full-house? sortedhand)
  (or (se-eq? (numeric-consec sortedhand) '(2 3)) (se-eq? (numeric-consec sortedhand) '(3 2))))

;           ((quads? sortedhand) 'QUADS)

(define (quads? sortedhand)
  (member? 4 (numeric-consec sortedhand)))

;           ((flush? sortedhand) 'FLUSH)

(define (flush? sortedhand)
  (same-suit? sortedhand))

;           ((straight? sortedhand) 'STRAIGHT)

(define (straight? sortedhand)
  (se-eq? (numeric-differences sortedhand) '(-1 -1 -1 -1)))

;           ((trips? sortedhand) 'TRIPS)

(define (trips? sortedhand)
  (member? 3 (numeric-consec sortedhand)))

;           ((two-pair? sortedhand) 'TWO-PAIR)

(define (two-pair? sortedhand)
  (and (member? 2 (numeric-consec sortedhand)) (member? 2 (bf (member 2 (numeric-consec sortedhand))))))

;           ((one-pair? sortedhand) 'PAIR)

(define (one-pair? sortedhand)
  (member? 2 (numeric-consec sortedhand)))

;           (else 'GARBAGE))))