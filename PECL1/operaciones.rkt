#lang racket

;funciones auxiliares
;===============================================================================
(define (get_m nodo) (car nodo))
(define (get_n nodo) (cadr nodo))
(define (get_p nodo) (caddr nodo))
(define (mcd m n) (if (= n 0) m (mcd n (remainder m n)) ))
(define (param_validos m n p) (and (< m n) (<= p m)) (= 1 (mcd m n)))

;operaciones
;===============================================================================
(define (llenar_m param nodo) (list (get_m param) (car (cdr nodo))))
(define (llenar_n param nodo) (list (car nodo) (get_n param)))

(define (vaciar_m param nodo) (list 0 (car (cdr nodo))))
(define (vaciar_n param nodo) (list (car nodo) 0))

(define (volcar_m param nodo)
    (if (>= (- (get_n param) (get_n nodo)) (get_m nodo))
        (list 0 (+ (get_m nodo) (get_n nodo)))
        (list (- (get_m nodo) (- (get_n param) (get_n nodo))) (get_n param))))
(define (volcar_n param nodo)
    (if (>= (- (get_m param) (get_m nodo)) (get_n nodo))
        (list (+ (get_m nodo) (get_n nodo)) 0)
        (list (get_m param) (- (get_n nodo) (- (get_m param) (get_m nodo))))))

;calculo de sucesores
;===============================================================================
;devuelve los sucesores que no han sido evaluados aún
(define (crear_supernodos padre nodos supernodos)
    (if (empty? nodos) supernodos
        (crear_supernodos padre 
            (cdr nodos) 
            (append supernodos (list (list (car nodos) padre))))))
(define (sucesores param nodo cerrados) 
    (crear_supernodos nodo (remove* cerrados (sucesores_aux param nodo)) null))

;calcula todos los sucesores de un nodo
(define (sucesores_aux param nodo) (list (llenar_m param nodo) 
    (llenar_n param nodo) (vaciar_m param nodo) (vaciar_n param nodo) 
    (volcar_m param nodo) (volcar_n param nodo)))

;funciones del algoritmo
;===============================================================================
;cerrados solo almacena el nodo, no el padre
;abiertos almacena el nodo y su padre (list nodo padre)
;arbol almacena el nodo y su padre (list nodo padre)
(define (algoritmo param abiertos cerrados arbol)
    (if (= (get_p param) (get_m (caar abiertos))) (append (list (car abiertos)) arbol)
        (algoritmo param (append (cdr abiertos) (sucesores param (caar abiertos) 
            (append cerrados (list (caar abiertos)))))
         (append cerrados (list (caar abiertos))) 
         (append (list (car abiertos)) arbol))
        )
    )

;devuelve la sublista que sigue a 'nodo'
(define (buscar nodo arbol) 
    (if (eq? (caar arbol) nodo) arbol
        (buscar nodo (cdr arbol))))

;obtiene la rama del arbol que lleva a la solucion
(define (calcular_ruta arbol ruta)
    (if (empty? (cadar arbol)) (append (list (caar arbol)) ruta)
        (calcular_ruta (buscar (cadar arbol) (cdr arbol))
            (append (list (caar arbol)) ruta))))

;manejadores
;===============================================================================
;i_m y i_n definen el estado inicial
(define (resolver m n p i_m i_n)
    (if (param_validos m n p) (calcular_ruta (algoritmo (list m n p) 
        (list(list (list i_m i_n) null)) null null) null)
        (printf "Parámetros no válidos")
        ))