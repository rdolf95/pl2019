#lang racket
(provide (all-defined-out))

(define (check_bst tree)
   (define (helper_check c_tree upper lower)
     (let
         ([value (car c_tree)]
          [left (car(cdr c_tree))]
          [right (car(cdr(cdr c_tree)))])
       (cond
         [(and (null? left) (null? right)) #t]
         [(null? left) 
          (if (and ( < (car right) upper) ( > (car right) value))
              (helper_check right upper value)
              #f)]
         [(null? right)
          (if (and ( > (car left) lower) ( < (car left) value))
              (helper_check left value lower)
		#f)]
         [#t (if (and 
                  (and ( < (car right) upper) ( > (car right) value))
                  (and ( > (car left) lower) ( < (car left) value)))
                 (and (helper_check right upper value) (helper_check left value lower))
                 #f)])))
	(helper_check tree +inf.0 -inf.0))

(define (apply f tree)
   (let ([value (car tree)]
        [left (car(cdr tree))]
        [right (car(cdr(cdr tree)))])
     (cond [(null? tree) '()]
           [(and (null? left) (null? right)) (list (f value) '() '())]
           [(null? left) (list(f value) '() (apply f right) )]
           [(null? right) (list(f value) (apply f left) '() )]
           [#t (list(f value) (apply f left) (apply f right))]
           )))


(define (equals tree1 tree2)
  (define (find val tree)
    (if (null? tree)
        #f
        (let ([value (car tree)]
              [left (car(cdr tree))]
              [right (car(cdr(cdr tree)))])
          (cond [(equal? val value) #t]
                [(< val value) (find val left)]
                [(> val value) (find val right)]
                ))))
  (define (compare treeA treeB)
    (if (null? treeA)
      #t
      (and (find (car treeA) treeB)
           (and (compare (car(cdr treeA)) treeB) (compare (car(cddr treeA)) treeB)))
      ))
  (if (and (check_bst tree1) (check_bst tree2))
  (and (compare tree1 tree2) (compare tree2 tree1))
  #f ))
    
                         
