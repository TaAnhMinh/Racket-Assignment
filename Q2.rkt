#lang racket
(define (sameNum e)
  (define L (reverse1 e))
  ;;c is the index of the highest number in the reduced list
  (define c (list-index (highest-number(duplicate L)) (duplicate L)))
  ;;b is the number that has the most consecutive dubpicate
  (define b (list-ref (remove L) c))
  ;;d is the highest number of continues duplicate
  (define d (highest-number(duplicate L)))
  (createList b d)
  )

(define (reverse1 l)
  (if (null? l)
     '()
     (append (reverse1 (cdr l)) (list (car l)))
  )
)
  
(define (createList B D)
  (cond
    ((> D 0) (cons B (createList B (- D 1))))
    (else '())))

(define (count3 L)
  (cond
    ((null? L) 0)
    ((= (length L) 1) 1)
    ((and (equal? (car L) (car (cdr L))) (= (length L) 2)) 2)
    ((equal? (car L) (car (cdr L))) (+ 1 (count3(cdr L))))
    (else 1)
    )
  )

(define (duplicate L)
  (cond
    ((null? L) '())
    ((and (>= (count3 L) 8) (>= (length L) 8)) (cons(count3 L) (duplicate (cddddr(cddddr L)))))
    ((and (>= (count3 L) 7) (>= (length L) 7)) (cons(count3 L) (duplicate (cdddr(cddddr L)))))
    ((and (>= (count3 L) 6) (>= (length L) 6)) (cons(count3 L) (duplicate (cddr(cddddr L)))))
    ((and (>= (count3 L) 5) (>= (length L) 5)) (cons(count3 L) (duplicate (cdr(cddddr L)))))
    ((and (>= (count3 L) 4) (>= (length L) 4)) (cons(count3 L) (duplicate (cddddr L))))
    ((and (>= (count3 L) 3) (>= (length L) 3)) (cons(count3 L) (duplicate (cdddr L))))
    ((and (>= (count3 L) 2) (>= (length L) 2)) (cons(count3 L) (duplicate (cddr L))))
    ((and (>= (count3 L) 1) (>= (length L) 1)) (cons(count3 L) (duplicate (cdr L))))
    (else '())
    )
  )
      

(define (list-ref lst n)
 (if (<= n 0)
     (car lst)
     (list-ref (cdr lst) (- n 1))))

(define (remove lst)
  (cond
    ((null? lst) '())
    ((null? (cdr lst)) lst)
    ((equal? (car lst) (car (cdr lst))) (remove (cdr lst)))
    (else (cons (car lst) (remove (cdr lst))))))

  (define (highest-number xs)
  (define (max x1 x2)
    (if (> x1 x2) x1 x2))
  (foldl max (first xs) (rest xs)))

(define list-index
        (lambda (e lst)
                (if (null? lst)
                        -1
                        (if (eq? (car lst) e)
                                0
                                (if (= (list-index e (cdr lst)) -1) 
                                        -1
                                        (+ 1 (list-index e (cdr lst))))))))
