#lang racket
(define (changeList L)
  (cond
    ((null? L) '())
    ((and (>= (car L) -1) (<= (car L) 1)) (changeList (cdr L)))
    ((< (car L) -1) (cons(/ 1 (* (car L) -1)) (changeList (cdr L))))
    (else ( cons (* (car L) 10) (changeList (cdr L))))
    )
  )

(define Q '(0 -2 3 -4 1))

(require racket/trace)
(trace changeList)

;;(changeList Q)

(define (filter pred lst)
  (reverse (filter-help pred lst '())))

(define (filter-help pred lst res)
  (cond ((null? lst) res)
        ((pred (car lst)) 
           (filter-help pred (cdr lst)  (cons (car lst) res)))
        (else 
           (filter-help pred (cdr lst)  res))))

(define (mymap f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (mymap f (cdr lst)))))


(define (changeList2 L)
  (map (lambda (x) (cond
                     ((and (>= x -1) (>= 1 x)) null)
                     ((< x 1) (/ 1 (* -1 x)))
                     ((> x 1) (* x 10))))
                      L)
  )

(changeList2 Q)

