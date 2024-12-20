; Before All

(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (leaf? node)
    (null? (children node)))

(define tree1 
    (make-node 'label-1 
        (list 
            (make-node 'label-2.1 
                (list  
                    (make-node 'label-3.1 null) 
                    (make-node 'label-3.2 null)
                    (make-node 'label-3.3 null)
                    (make-node 'label-3.4
                        (list
                            (make-node 'label-4.1 null)))))
            (make-node 'label-2.2
                (list
                    (make-node 'label-3.5 null))))))


"End of 'Header File'"
; 18.1  What does
; ((SAN FRANCISCO))
; mean in the printout of world-tree? Why two sets of parentheses?

"It means that SAN FRANCISCO is a city of a state of the world"
"In other words, it's a leaf node with a depth of 2"

; 18.2  Suppose we change the definition of the tree constructor so that it uses list instead of cons:
; How do we have to change the selectors so that everything still works?
; -- We replace:
"OLD USELESS PROCEDURES"
(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))
; -- With these new procedures.
"NEW USEFUL HELPFUL EXCELLENT PROCEDURES"
(define (make-node datum children)
  (list datum children))

(define (datum node)
  (list-ref node 0))

(define (children node)
  (list-ref node 1))

; 18.3  Write depth, a procedure that 
; takes a tree as argument and 
; returns the largest number of nodes connected through parent-child links. 
; That is, a leaf node has depth 1
; a tree in which all the children of the root node are leaves has depth 2. 
; Our world tree has depth 4 
; (because the longest path from the root to a leaf is, for example, world, country, state, city).

"Suppose the node is cons style"

(define (get-depth node)
    (cond ((leaf? node) 1)
          (else (+ 1 (reduce max (map get-depth (children node)))))))

; 18.4  Write count-nodes, a procedure that 
; takes a tree as argument and returns the total number of nodes in the tree. 
; (Earlier we counted the number of leaf nodes.)

(define (count-nodes node)
    (cond ((leaf? node) 1)
          (else (+ 1 (reduce + (map count-nodes (children node)))))))

; 18.5  Write prune, a procedure that 
; takes a tree as argument and returns a copy of the tree, 
; but with all the leaf nodes of the original tree removed. 
; (If the argument to prune is a one-node tree, 
;  in which the root node has no children, 
;  then prune should return #f because the result of removing the root node wouldn't be a tree.)

(define (prune node)
    (cond ((leaf? node) #f)
          (else (prune-helper node))))

(define (prune-helper tree)
    (cond ((leaf? tree) null)
          (else (make-node (datum tree) (keep (lambda (el) (not (null? el))) (map prune-helper (children tree)))))))

; 18.6  Write a program parse-scheme that 
; parses a Scheme arithmetic expression 
; into the same kind of tree that parse produces for infix expressions. 
; Assume that all procedure invocations in the Scheme expression have two arguments.
; The resulting tree should be a valid argument to compute:
; > (compute (parse-scheme '(* (+ 4 3) 2)))
; 14

; Achieve: 1 2 3
;            2?3
;          1?(2?3)

(define (mapping op)
  (cond ((eq? op '+) +)
        ((eq? op '-) -)
        ((eq? op '*) *)
        ((eq? op '/) /)
        (else #f)))

(define (parse-scheme input)
  (let*
    ((parsed-input (map (lambda (el) (if (list? el) (parse-scheme el) el)) input))
     (op (mapping (car parsed-input)))
     (args (cdr parsed-input)))
        (reduce-l2r op args)))

(define (reduce-l2r op lst)
  (cond ((= 1 (length lst)) (car lst))
        (else (reduce-l2r op (cons (op (car lst) (cadr lst)) (cddr lst))))))