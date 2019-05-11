;;2015004575 JunHyeok Bae sol4
#lang racket
(provide (all-defined-out))

(define (dataOf node) (car node))
(define (leftChildOf node) (cadr node))
(define (rightChildOf node) (caddr node))

(define (check_bst rt)
  (if (null? rt)#t
      (letrec ([data (dataOf rt)]
            [checkL (lambda(node)
                      (if (null? node) #t
                          (> data (dataOf node))))]
            [checkR (lambda(node)
                      (if (null? node) #t
                          (< data (dataOf node))))])
        (and (checkL (leftChildOf rt)) (checkR (rightChildOf rt)) (check_bst (leftChildOf rt)) (check_bst (rightChildOf rt))))))


(define (apply func rt)
  (if (null? rt) null
      (list (func (dataOf rt)) (apply func (leftChildOf rt)) (apply func (rightChildOf rt)))))


(define (equals rt1 rt2)
  (letrec ([isExist (lambda(data rt)
                   (if (null? rt) #f
                   (or (= data (dataOf rt)) (isExist data (leftChildOf rt)) (isExist data (rightChildOf rt)))))]
           [cmp (lambda(rtS rtT)
                 (if (null? rtS) #t
                 (and (isExist (dataOf rtS) rtT) (cmp (leftChildOf rtS) rtT) (cmp (rightChildOf rtS) rtT))))])
    (and (cmp rt1 rt2) (cmp rt2 rt1))))

;;testcase

#|
(check_bst '(6 (4 ()()) (7 ()())))
(check_bst '(6(7 ()()) (8 ()())))
(apply (lambda (v) (+ v 1)) '(7 (6 ()()) (8 ()())))
(check_bst '(8 (7 () ()) (9 () ())))
(equals '(7 (6 ()()) (8 ()())) '(6 () (7 () (8 ()()))))
(equals '(7 (6()()) (8 ()())) '(7 (6 ()()) (8 ()(9 () ()))))
|#
