#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)

(define (racketlist->mupllist xs)
  (cond [(null? xs) (aunit)]
        [#t (apair (car xs) (racketlist->mupllist (cdr xs)))]))

(define (mupllist->racketlist es)
  (cond [(aunit? es) '()]
        [#t (cons (apair-e1 es) (mupllist->racketlist (apair-e2 es)))]))

;(define (racketlist->mupllist xs)
;  (cond [(null? xs) (aunit)]
;        [(string? (car xs)) (apair (var (car xs)) (racketlist->mupllist (cdr xs)))]
;        [(number? (car xs)) (apair (int (car xs)) (racketlist->mupllist (cdr xs)))]
;        [(list? (car xs)) (apair (racketlist->mupllist (car xs)) (racketlist->mupllist (cdr xs)))]))

;(define (mupllist->racketlist es)
;  (cond [(aunit? es) '()]
;        [(var? (fst (es))) (cons (var-string ((fst (es)))) (mupllist->racketlist (snd (es))))]
;        [(int? (fst (es))) (cons (int-num ((fst (es)))) (mupllist->racketlist (snd (es))))]
;        [(pair?(fst (es))) (cons (mupllist->racketlist (fst (es))) (mupllist->racketlist (snd (es))))]
;        [(aunit? (fst es)) (cons ('() (mupllist->racketlist (snd (es)))))]))
       ;; closure?

                        
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL if greater applied to non-number")))]
        [(fun? e)
         (closure env e)]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([v (eval-under-env (call-funexp e) env)])
           (if (closure? v)
               (let([arg (eval-under-env (call-actual e) env)]
                    [fn (closure-fun v)])
                 (eval-under-env (fun-body fn)
                                 (cons (cons (fun-formal fn) arg)
                                       (cons (cons (fun-nameopt fn) v) (closure-env v)))))
               (error "MUPL call applied to non-closure")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(aunit? e) e]
        [(closure? e) e]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [#t (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))]))
  

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y") e4
                         (ifgreater (var "_y") (var "_x") e4 e3)))))
                                    
                                    

;; Problem 4

(define mupl-map
  (fun #f "f"
       (fun "real_map" "es"
            (ifeq (isaunit (var "es")) (int 1) (aunit)
                  (apair (call (var "f") (fst (var "es"))) (call (var "real_map") (snd (var "es"))))))))   

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "int_arg"
             (call (var "map") (fun #f "list_e" (add (var "int_arg") (var "list_e")))))))

       
;"CHANGE (notice map is now in MUPL scope)"


(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
