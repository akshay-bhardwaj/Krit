;; @Auther: Akshay Bhardwaj
;; @Details: New programming language developed while undergoing a programming course
;; This file is part of Krit.

;;    Foobar is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.

;;    Foobar is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.

;;    You should have received a copy of the GNU General Public License
;;    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

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

;; Part I

(define (racketlist->mupllist rl) 
  (if (null? rl)
      (aunit)
      (cond [(integer? (car rl)) (cons (int (car rl)) (racketlist->mupllist (cdr rl)))]
            [(string? (car rl)) (cons (var (car rl)) (racketlist->mupllist (cdr rl)))]
            [(pair? (car rl)) (cons (cons (racketlist->mupllist (car  (car rl)))
                                          (racketlist->mupllist (car (cdr (car rl)))))
                                    (racketlist->mupllist (cdr rl)))]
            )))
            
(define (mupllist->racketlist rl)
  (if (aunit? rl)
      null
      (cond [(int? (car rl)) (cons (int-num (car rl)) (mupllist->racketlist (cdr rl)))]
            [(var? (car rl)) (cons (var-string (car rl)) (mupllist->racketlist (cdr rl)))]
            [(pair? (car rl)) (cons (cons (mupllist->racketlist (car (car rl)))
                                          (mupllist->racketlist (car (cdr (car rl)))))
                                    (mupllist->racketlist (cdr rl)))]
            )))

                                          
                             
                                 

;; Part II

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
        ;; CHANGE add more cases here
        [(closure? e) e]
	[(int? e) e]
	[(fun? e) (closure env e)] 
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env))
               (error "Addition of Non integers"))
               
               )]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons mlet-var v) env)))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([cfun (closure-fun v1)]
                     [cenv (closure-env v1)]
                     [fname (cons (fun-nameopt cfun) v1)]
                     [fparm (cons (fun-formal cfun) v2)]
                     [fenv (cons fparm
                                 (if (fun-nameopt cfun)
                                     (cons fname cenv)
                                     cenv))])
                 (eval-under-env (fun-body cfun) fenv))
               (error "Cannot be applied to non-closure")))]
            
       
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (cons v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "Not a pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "Not a pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) 1 0))]
           
        [#t (error "bad MUPL expression")]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem III

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) 
  (if (null? lstlst)
      (e2)
      (mlet (car (car lstlst)) (car (cdr (car lstlst))) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4) (ifgreater e1 e2 e4 (ifgreater e2 e1 e4 e3)))

;; Problem 4

(define mupl-map
  (fun #f "func" 
       (fun "f" "xs" 
            (ifaunit (var "xs")
                       (aunit)
                       (apair (call (var "func") (fst (var "xs"))) 
                              (call (var "f") (snd (var "xs"))))))))
                                              

(define mupl-mapAddN 
  (mlet "map" mupl-map
       (fun #f "i" 
            (call (var "map") 
                  (fun #f "x" (add (var "i") (var "x")))))))
;; part IV

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
