#lang racket

(provide (all-defined-out))

; LIST IMPLEMENTATION
; helper functions that make lists where first element is a symbol
(define (Const i) (list 'Const i)) ; => ('Const i)
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2)) ; => (Add (Const 10) 42)
(define (Multiply e1 e2) (list 'Multiply e1 e2))

;(Multiply (Add (Const 10) (Negate (Const 42))) (Const 10))

; helper functions that test what "kind of exp"
(define (Const? x) (eq? (car x) 'Const))
(define (Negate? x) (eq? (car x) 'Negate))
(define (Add? x) (eq? (car x) 'Add))
(define (Multiply? x) (eq? (car x) 'Multiply))

; helper functions that get the pieces for "one kind of exp"
; ('Const 42)
(define (Const-int e) (car (cdr e)))
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))
(define (Multiply-e1 e) (car (cdr e)))
(define (Multiply-e2 e) (car (cdr (cdr e))))

(Negate (Negate (Const 42)))

;(list 'Negate (list 'Negate (list 'Const 42)))

; same recursive structure as we have in ML
; one change: returning an exp rather than an int
;(Negate (Const 10))

(define (eval-exp1 e)
  (cond [(Const? e) e] ; note returning an exp, not a number
        [(Negate? e) (Const (- (Const-int (eval-exp1 (Negate-e e)))))]
        [(Add? e) (let ([v1 (Const-int (eval-exp1 (Add-e1 e)))]
                        [v2 (Const-int (eval-exp1 (Add-e2 e)))])
                    (Const (+ v1 v2)))]  ; ('Const v1 v2)
        [(Multiply? e) (let ([v1 (Const-int (eval-exp1 (Multiply-e1 e)))]
                             [v2 (Const-int (eval-exp1 (Multiply-e2 e)))])
                         (Const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))

(define a-test1 (eval-exp1 (Multiply (Negate (Add (Const 2) (Const 2))) (Const 7))))
;;a-test1 ==> ('Const -28)

; STRUCT IMPLEMENTATION

; same idea using Racket's structs, which are more convenient /and/ less
; error-prone

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)


(define (eval-exp2 e)
  (cond [(const? e) e] ; note returning an exp, not a number
        [(negate? e) (const (- (const-int (eval-exp2 (negate-e e)))))]
        [(add? e) (let ([v1 (const-int (eval-exp2 (add-e1 e)))]
                        [v2 (const-int (eval-exp2 (add-e2 e)))])
                    (const (+ v1 v2)))]
        [(multiply? e) (let ([v1 (const-int (eval-exp2 (multiply-e1 e)))]
                             [v2 (const-int (eval-exp2 (multiply-e2 e)))])
                         (const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))

;(define a-test2 (eval-exp2 (multiply (negate (add (const 2) (const 2))) (const 7))))



;struct to represent ast
(struct const (int) #:transparent) ; int should hold a number
(struct negate (e1) #:transparent)  ; e1 should hold an expression
(struct add (e1 e2) #:transparent) ; e1, e2 should hold expressions
(struct multiply (e1 e2) #:transparent) ; e1, e2 should hold expressions
(struct bool (b) #:transparent) ; b should hold #t or #f
(struct eq-num (e1 e2) #:transparent) ; e1, e2 should hold expressions
(struct if-then-else (e1 e2 e3) #:transparent) ; e1, e2, e3 should hold expressions

; a value in this language is [const bool]

(define test1 (multiply (negate (add (const 2) 
                                     (const 2))) 
                        (const 7)))

(define test2 (multiply (negate (add (const 2)
                                     (const 2))) 
                        (if-then-else (bool #f) 
                                      (const 7) 
                                      (bool #t))))

(define non-test (multiply (negate (add (const #t) 
                                        (const 2))) 
                           (const 7)))


; what's wrong? [________________]
(define (eval-exp-wrong e)
  (cond [(const? e) 
         e] 
        [(negate? e) 
         (const (- (const-int (eval-exp-wrong (negate-e1 e)))))]
        [(add? e) 
         (let ([i1 (const-int (eval-exp-wrong (add-e1 e)))]
               [i2 (const-int (eval-exp-wrong (add-e2 e)))])
           (const (+ i1 i2)))]
        [(multiply? e) 
         (let ([i1 (const-int (eval-exp-wrong (multiply-e1 e)))]
               [i2 (const-int (eval-exp-wrong (multiply-e2 e)))])
           (const (* i1 i2)))]
        [(bool? e) 
         e]
        [(eq-num? e) 
         (let ([i1 (const-int (eval-exp-wrong (eq-num-e1 e)))]
               [i2 (const-int (eval-exp-wrong (eq-num-e2 e)))])
           (bool (= i1 i2)))] ; creates (bool #t) or (bool #f)
        [(if-then-else? e)
         (if (bool-b (eval-exp-wrong (if-then-else-e1 e)))
             (eval-exp-wrong (if-then-else-e2 e))
             (eval-exp-wrong (if-then-else-e3 e)))]
        [#t (error "eval-exp expected an exp")] ; not strictly necessary but helps debugging
        ))





;correct one
(define e1 (if-then-else (const 42) (const 43) (const 44)))
(define (eval-exp e)
  (cond [(const? e) 
         e] 
        [(negate? e) 
         (let ([v (eval-exp (negate-e1 e))])
           (if (const? v)
               (const (- (const-int v)))
               (error "negate applied to non-number")))]
        [(add? e) 
         (let ([v1 (eval-exp (add-e1 e))]
               [v2 (eval-exp (add-e2 e))])
           (if (and (const? v1) (const? v2))
               (const (+ (const-int v1) (const-int v2)))
               (error "add applied to non-number")))]
        [(multiply? e) 
         (let ([v1 (eval-exp (multiply-e1 e))]
               [v2 (eval-exp (multiply-e2 e))])
           (if (and (const? v1) (const? v2))
               (const (* (const-int v1) (const-int v2)))
               ((error "multiply applied to non-number"))))]
        [(bool? e) 
         e]
        [(eq-num? e) 
         (let ([v1 (eval-exp (eq-num-e1 e))]
               [v2 (eval-exp (eq-num-e2 e))])
           (if (and (const? v1) (const? v2))
               (bool (= (const-int v1) (const-int v2))) ; creates (bool #t) or (bool #f)
               (error "eq-num applied to non-number")))]
        [(if-then-else? e) 
         (let ([v-test (eval-exp (if-then-else-e1 e))])
           (if (bool? v-test)
               (if (bool-b v-test)
                   (eval-exp (if-then-else-e2 e))
                   (eval-exp (if-then-else-e3 e)))
               (error "if-then-else applied to non-boolean")))]
        [#t (error "eval-exp expected an exp")] ; not strictly necessary but helps debugging
        ))

; Example of a macro implemented in Racket functions 
; Notice that it returns an AST
(define (andalso e1 e2)
  (if-then-else e1 e2 (bool #f)))

(define (double e)
  (multiply e (const 2)))

; Another macro; given (list-products v1 v2 v3 ...) 
;            it expands to (multiply v1 (multiply (v2 (multiply v3 ...))))
(define (list-product es)
  (if (null? es)
      (const 1)
      (multiply (car es) (list-product (cdr es)))))

; the following AST contains a sub-AST created by macros.
(define test (andalso (eq-num (double (const 4))
                              (list-product (list (const 2) (const 2) (const 1) (const 2))))
                      (bool #t)))

; notice we have not changed our interpreter at all
(define result (eval-exp test))

; CONTRACT

(provide (contract-out 
           [amount positive?]
           [deposit (-> number? any)] ; number? -> any
           [balance (-> number?)]))


(define amount 0)
(define (deposit a) (set! amount (+ amount a)))
(define (balance) amount)
