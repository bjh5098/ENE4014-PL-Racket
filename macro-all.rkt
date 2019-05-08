#lang racket
;; a loop that executes body hi - lo times
;; notice use of local variables

(define-syntax dbl
  (syntax-rules ()
    [(dbl x)
     (let ([v x])
       (+ v v))]))

(define-syntax dbl2
  (syntax-rules ()
    [(dbl x) (* 2 x)]))


; (for 5 to 10 do (+ a b))
(define-syntax for
  (syntax-rules (to do)
    [(for lo to hi do body)
     (let ([h hi]
           [l lo])
       (letrec ([loop (lambda (it)
                        (if (> it h)
                            #t
                            (begin body (loop (+ it 1)))))])
         (loop l)))]))

;; let2 allows up to two local bindings (with let* semantics) with fewer parentheses
;; than let*
(define-syntax let2
  (syntax-rules ()
    [(let2 () body)
     body]
    [(let2 (var val) body)
     (let ([var val]) body)]
    [(let2 (var1 val1 var2 val2) body)
     (let ([var1 val1])
       (let ([var2 val2])
         body))]))

;; the special ... lets us take any number of arguments
;; Note: nothing prevents infinite code generation except
;; the macro definer being careful
; (my-let* ([a 10]
;           [b 42]
;           [c "abc"])
;         (+ a b))
(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body)
     body]

    [(my-let* ([var0 val0]
               [var-rest val-rest] ...)
              body)
     (let ([var0 val0])
       (my-let* ([var-rest val-rest] ...)
                body))]))




;(mylist a b c) ==> ('a 'b 'c)
(define-syntax mylist
  (syntax-rules ()
    [(mylist) (list)]
    [(mylist e) (list (quote e))]
    [(mylist e1 e2 ...)
     (cons (quote e1) (mylist e2 ...))]))

; (rotate a b c) => ('b 'c 'a)
(define-syntax rotate
  (syntax-rules ()
    [(rotate) ()]
    [(rotate a) '(a)]    
    [(rotate a b ...) (append (mylist b ...) '(a))]))

(define db
  '(("a" . 10) ("b" . 11) ("c" . 12) ("d" . 13) ("e" . 14)))

(define-syntax select
  (syntax-rules (key value from where)
    [(select from e1 where key = e2)
     (assoc e2 e1)]
    [(select key from e1 where key = e2)
     (map (lambda (kv) (car kv)) (assoc e2 e1))]    
    [(select value from e1 where key = e2)
     (map (lambda (kv) (cdr kv)) (assoc e2 e1))]    
    [(select from e1 where value = e2)
     (filter-not null? (map (lambda (kv) (if (= (cdr kv) e2) kv null)) e1))]
    ))
