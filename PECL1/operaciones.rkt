#lang racket
(define m 3)
(define n 4)
(define p 1)

;funciones auxiliares
(define (get_m nodo) (car nodo))
(define (get_n nodo) (car (cdr nodo)))
(define (param_validos m n p) (and (< m n) (<= p m)))

;operaciones
(define (llenar_m nodo) (list m (car (cdr nodo))))
(define (llenar_n nodo) (list (car nodo) n))

(define (vaciar_m nodo) (list 0 (car (cdr nodo))))
(define (vaciar_n nodo) (list (car nodo) 0))

(define (volcar_m nodo)
    (if (>= (- n (get_n nodo)) (get_m nodo))
        (list 0 (+ (get_m nodo) (get_n nodo)))
        (list (- (get_m nodo) (- n (get_n nodo))) n)))
(define (volcar_n nodo)
    (if (>= (- m (get_m nodo)) (get_n nodo))
        (list (+ (get_m nodo) (get_n nodo)) 0)
        (list m (- (get_n nodo) (- m (get_m nodo))))))

(define (operaciones nodo) (list 
    (llenar_m nodo)
    (llenar_n nodo)
    (vaciar_m nodo)
    (vaciar_n nodo)
    (volcar_m nodo)
    (volcar_n nodo)))
;funciones del algoritmo
;(define abiertos (list (list 0 0)))

(define (algoritmo abiertos cerrados solucion)
    (if (= p (get_m (car abiertos))) (car abiertos)
        (algoritmo (append (cdr abiertos) (operaciones (car abiertos))) empty empty))
    )

(define test (algoritmo (list (list 0 0)) empty empty))