

















(define (deep-member? target lst)
    (member? target (flatten-list lst)))

(define (flatten-list lst)
    (if (not (ormap list? lst)) lst
    (flatten-list (reduce append (map (lambda (x) (if (list? x) x (list x)) ) lst)))))