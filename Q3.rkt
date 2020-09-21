#lang racket
(define choices '('("marie" '("peru" "greece" "vietnam"))
'("jean" '("greece" "peru" "vietnam"))
'("sasha" '("vietnam" "peru" "greece"))
'("helena" '("peru" "vietnam" "greece"))
'("emma" '("greece" "peru" "vietnam"))
'("jane" '("greece" "vietnam" "peru"))))

(define choices2 '('("marie" '("peru" "greece" "vietnam"))
'("jean" '("greece" "peru" "vietnam"))
'("sasha" '("vietnam" "peru" "greece"))
'("helena" '("peru" "vietnam" "greece"))
'("emma" '("greece" "peru" "vietnam"))))



(define (destination L )
  (define p (findPeru L))
  (define g (findGreece L))
  (define v (findVietnam L))
  (define a '("Peru" "Greece" "Vietnam"))
  (define b (list p g v)) ;;b is a list of numbers
  (define c (highest-number b)) ;; c is the highest number in the list b
  (define A (positions c b 0)) ;;A indicate the indexes of the highest number Ex output: (0 1)
  ;;where the show function will call.
  (show A a c)
 )

(define (show LI LE H) ;;show function take input list index (indexes) and list element '("Peru" "Greece" "Vietnam")
  (cond
    ((null? (cdr LI)) ( cons(list-ref LE (car LI))(cons H '())))
    (else (cons (list-ref LE (car LI))  (show (cdr LI) LE H)))
    )
  )

(define positions
  (lambda (A L i)
    (if (null? L)
        '()                                       ; not found
        (if (equal? (car L) A)
            (cons i                               ; found at index i
                  (positions A (cdr L) (+ i 1)))  ;   and continue
            (positions A (cdr L) (+ i 1)))))) 

(define-syntax forloop
  (syntax-rules ()
    ((forloop start stop exps ...)
     (let ((j stop))
       (let loop ((i start))
         (when (<= i j)
           exps ...
           (loop (+ i 1))))))))
 
(define (findPeru L)
  (cond
    ((null? L) 0)
    ;;if peru in the first coloumn plus 3 points
    ((equal? (caadar (cdadar L)) "peru") (+ 3 (findPeru(cdr L))))
    ;;if peru in the second coloumn plus 2 points
    ((equal? (car(cdadar (cdadar L))) "peru") (+ 2 (findPeru(cdr L))))
    ;;if peru in the third coloumn plus 1 points
    ((equal? (cadr(cdadar (cdadar L))) "peru") (+ 1 (findPeru(cdr L))))
    (else 0)
    )
  )

(define (findGreece L)
  (cond
    ((null? L) 0)
    ;;if peru in the first coloumn plus 3 points
    ((equal? (caadar (cdadar L)) "greece") (+ 3 (findGreece(cdr L))))
    ;;if peru in the second coloumn plus 2 points
    ((equal? (car(cdadar (cdadar L))) "greece") (+ 2 (findGreece(cdr L))))
    ;;if peru in the third coloumn plus 1 points
    ((equal? (cadr(cdadar (cdadar L))) "greece") (+ 1 (findGreece(cdr L))))
    (else 0)
    )
  )

(define (findVietnam L)
  (cond
    ((null? L) 0)
    ;;if peru in the first coloumn plus 3 points
    ((equal? (caadar (cdadar L)) "vietnam") (+ 3 (findVietnam(cdr L))))
    ;;if peru in the second coloumn plus 2 points
    ((equal? (car(cdadar (cdadar L))) "vietnam") (+ 2 (findVietnam(cdr L))))
    ;;if peru in the third coloumn plus 1 points
    ((equal? (cadr(cdadar (cdadar L))) "vietnam") (+ 1 (findVietnam(cdr L))))
    (else 0)
    )
  )

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

(define (list-ref lst n)
 (if (<= n 0)
     (car lst)
     (list-ref (cdr lst) (- n 1))))

(require racket/trace)
;;(trace findPeru)
;;(trace findGreece)
;;(trace findVietnam)
(trace destination)
                     
(destination choices)

;;(car (car (cdr (car choices)))) return "marie" || (caadar choices)
;;(car(cdr (car (cdr (car (cdr (car choices)))))))) return list ("peru" "greece" "vietnam") || (cadar (cdadar choices))
;;(car(car(cdr (car (cdr (car (cdr (car choices)))))))) return peru || (caadar (cdadar choices))
;;(car(cdr(car(cdr (car (cdr (car (cdr (car choices))))))))) return greece || (car(cdadar (cdadar choices)))
;;(car(cdr(cdr(car(cdr (car (cdr (car (cdr (car choices)))))))))) return vietnam || (cadr(cdadar (cdadar choices)))