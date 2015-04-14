#lang racket
(define m 3)
(define n 4)
(define p 2)

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

;devuelve los sucesores que no han sido evaluados aún
(define (crear_supernodos padre nodos supernodos)
    (if (empty? nodos) supernodos
        (crear_supernodos padre 
            (cdr nodos) 
            (append supernodos (list (list (car nodos) padre))))))
(define (sucesores nodo cerrados) 
    (crear_supernodos nodo (remove* cerrados (sucesores_aux nodo)) null))

;calcula todos los sucesores de un nodo
(define (sucesores_aux nodo) (list (llenar_m nodo) (llenar_n nodo) 
    (vaciar_m nodo) (vaciar_n nodo) (volcar_m nodo) (volcar_n nodo)))

;funciones del algoritmo
;===============================================================================
;cerrados solo almacena el nodo, no el padre
;abiertos almacena el nodo y su padre (list nodo padre)
;arbol almacena el nodo y su padre (list nodo padre)
(define (algoritmo abiertos cerrados arbol)
    (if (= p (get_m (caar abiertos))) (append (list (car abiertos)) arbol)
        (algoritmo (append (cdr abiertos) (sucesores (caar abiertos) 
            (append cerrados (list (caar abiertos)))))
         (append cerrados (list (caar abiertos))) 
         (append (list (car abiertos)) arbol))
        )
    )

;devuelve la sublista que sigue al nodo
(define (buscar nodo arbol) 
    (if (eq? (caar arbol) nodo) arbol
        (buscar nodo (cdr arbol))))

(define (calcular_ruta arbol ruta)
    (if (empty? (cadar arbol)) (append (list (caar arbol)) ruta)
        (calcular_ruta (buscar (cadar arbol) (cdr arbol))
            (append (list (caar arbol)) ruta))))

(define resolver (calcular_ruta (algoritmo (list (list (list 0 0) null)) 
    empty empty) null))

(define test (algoritmo (list (list (list 1 1) null)) empty empty))