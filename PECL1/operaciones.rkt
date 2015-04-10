#lang racket
(define m 3)
(define n 4)
(define p 2)
(define (get_m nodo) (car nodo))
(define (get_n nodo) (car (cdr nodo)))


(define (param_validos m n p) (and (< m n) (<= p m)))

(define (llenar_m nodo) (list m (car (cdr nodo))))

(define (llenar_n nodo) (list (car nodo) n))

(define (vaciar_m nodo) (list 0 (car (cdr nodo))))

(define (vaciar_n nodo) (list (car nodo) 0))

; esta bien, (2 3) ya funciona
(define (volcar_m nodo)
    (if (>= (- n (get_n nodo)) (get_m nodo))
        (list 0 (+ (get_m nodo) (get_n nodo)))
        (list (- (get_m nodo) (- n (get_n nodo))) n)))

(define (volcar_n nodo)
    (if (>= (- m (get_m nodo)) (get_n nodo))
        (list (+ (get_m nodo) (get_n nodo)) 0)
        (list m (- (get_n nodo) (- m (get_m nodo))))))


;(define (algoritmo nodos))