#lang racket
;;Q1
(define (sigmoid v)
  (/ 1 (+ 1 (exp (* -1 v)))))

;;calculate Z
(define (neuralNode L S)
  (lambda (X)
    (S (+ (car L) (* (car X) (car(cdr L))) (* (car (cdr X)) (car(cdr (cdr L)))) ))
    )
  )
;;((neuralNode '(0.1 0.3 0.4) sigmoid) '(0.5 0.5))

;;Q2
; 2 X are consistent, fixed.
; ((neuralLayer '((0.1 0.3 0.4)(0.5 0.8 0.3)(0.7 0.6 0.6))) '(0.5 0.5))
;Output: '(0.610639233949222 0.740774899182154 0.7858349830425586)
(define (neuralLayer L)
  (lambda (X)
    (cond
      ((null? L) '())
      (else (cons ((neuralNode (car L) sigmoid) X) ((neuralLayer (cdr L)) X)))
      )
    )
  )

;;Q3
(define (neuralNet X)
  (list (neuralNet2 X sigmoid))
)

(define (neuralNet2 X S)
  (define L ((neuralLayer '((0.1 0.3 0.4)(0.5 0.8 0.3)(0.7 0.6 0.6))) X))
  (S (+ 0.5 (* 0.3 (car L)) (* 0.7 (car(cdr L))) (* 0.1 (car(cdr(cdr L))))))
  )



;Q4
;;(applyNet 16)
(define (applyNet N)
  (define L (generateL 1 N))
  (sol L N)
  )

(define (sol L N)
  (cond
    ((null? L) '())
    (else (cons (neuralNet (getX (car L) N )) (sol (cdr L) N)))
    )
  )
           
(define (getX K N)
  (list (X1 K N) (X2 K N))
  )
(define (generateL C N)
  (cond
    ((= C (+ N 1)) '())
    (else (cons C (generateL (+ C 1) N)))
    )
  )

(define pi 3.14159)

(define (X1 K N)
  (sin (/ (* 2 pi (- K 1)) N))
  )

(define (X2 K N)
  (cos (/ (* 2 pi (- K 1)) N))
  )

(require racket/trace)







