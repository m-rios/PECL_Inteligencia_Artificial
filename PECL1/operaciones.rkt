#lang racket
(define m 3)
(define n 4)
(define p 2)

(define (param_validos m n p)
    (and (< m n) (<= p m)))

(define (llenar_m nodo)
    (list m (car (cdr nodo))))

(define (llenar_n nodo)
    (list (car nodo) n))

; esta mál, (2 3) no funciona
(define (volcar_m_en_n nodo)
    (if (>= m (car (cdr nodo)))
        (list (car (cdr nodo)) 0)
        (list m (- (car (cdr nodo)) (- m(car nodo))))))