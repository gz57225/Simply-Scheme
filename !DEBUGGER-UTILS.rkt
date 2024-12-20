(define debug-level 'debug)


(define color-codes
  '((debug . "\033[34m")   
    (info . "\033[32m")    
    (warning . "\033[33m") 
    (error . "\033[31m")   
    (fault . "\033[35m")   
    (reset . "\033[0m")))  

(define (logger level message i)
  (let ((levels '(debug info warning error fault)))
    (if (>= (index level levels) (index debug-level levels))
        (begin
          (display (cdr (assoc level color-codes))) 
          (display "[")
          (display (string-upcase (symbol->string level)))
          (display "] ")
          (display message)
          (display " ")
          (display i)
          (display " [END]\n")
          (display (cdr (assoc 'reset color-codes))))
        null)))

(define (index item lst)
  (let loop ((lst lst) (i 0))
    (cond ((null? lst) #f)
          ((equal? (car lst) item) i)
          (else (loop (cdr lst) (+ i 1))))))

(logger 'debug "This is a debug message" 42)
(logger 'info "This is an info message" 42)
(logger 'warning "This is a warning message" 42)
(logger 'error "This is an error message" 42)
(logger 'fault "This is a fault message" 42)