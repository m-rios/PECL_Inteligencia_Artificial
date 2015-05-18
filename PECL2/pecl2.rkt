;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pecl2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;devuelve el tamaño en mm de cada subdivisión
(define (dinA_mm n)
  (map (lambda (number1 number2)
         (quotient number1 number2)) '(841 1189) (dinA n)))

;devuelve las divisiones del papel original según la n
(define (dinA n) (dinAux n (list 1 1) 0))
;función recursiva
(define (dinAux n p c)
  (if (= c n)
      p
      (if (odd? c)
          ;si impar duplicar componente x
          (dinAux n (list (* 2 (car p)) (cadr p)) (+ 1 c))
          ;si par duplicar componente y
          (dinAux n (list (car p) (* 2 (cadr p))) (+ 1 c))
          )
      )
  )

;codificación del nodo_______________________________________________________________
;nodo = ((tablero), tipo, puntuacion)
;tipo: 0->max 1->min
;devuelve el tablero
(define (get-tab nodo) (first nodo))
;devuelve si es min o max
(define (get-minmax nodo) (second nodo))
;devuelve 0 si no ha sido explotado o 1 si no tiene valor
(define (get-tipo nodo) (third nodo))
;devuelve la puntuación
(define (get-pun nodo) (fourth nodo))

;codificación del algoritmo
;minimo de una lista
(define (min-l lista) (car (sort lista <)))
;maximo de una lista
(define (max-l lista) (car (sort lista >)))
;calcula los hijos de un nodo
(define (explotar nodo)
  (append (explotar-izq nodo 1 '() (first (get-tab nodo))) ;hijos jugando por la izquierda
          (explotar-der nodo 1 '() (second (get-tab nodo))) ;hijos jugando por la derecha
          (list (list (get-tab nodo) (get-minmax nodo) -1 (get-pun nodo)));mismo nodo, para hacer backtracking
          ))

(define (explotar-izq nodo n hijos max)
  (if (= n max)
      hijos
      ((lambda (x) (explotar-izq x (+ n 1) (append hijos (list x)) max)) 
       (list 
        (list (- (first (get-tab nodo)) n) (second (get-tab nodo)));tab
        (remainder (+ 1 (get-minmax nodo)) 2); min-max
        -2 ;no explotado
        -1 ;no tiene puntuacion
        ))
      ))
(define (explotar-der nodo n hijos max)
  (if (= n max)
      hijos
      ((lambda (x) (explotar-izq x (+ n 1) (append hijos (list x)) max)) 
       (list 
        (list (first (get-tab nodo)) (- (second (get-tab nodo)) n));tab
        (remainder (+ 1 (get-minmax nodo)) 2); min-max
        -2 ;no explotado
        -1 ;no tiene puntuacion
        ))
      ))
                                                                                   
              

;plantar crea el árbol de decisiones
(define (plantar actual abiertos puntuaciones arbol)
  (if (and (empty? abiertos) (not(empty? arbol))) ;si abiertos vacio y arbol no vacio, casi se ha terminado el árbol
      (cons 
       (list (get-tab actual) (get-minmax actual) (get-tipo actual) (min-l (car puntuaciones))) arbol)    ;igual en vez de (list arbol) es arbol solo
      (if (equal? '(1 1) (get-tab actual))
          (plantar (car abiertos);actual'
                   (cdr abiertos);abiertos'
                   (cons (cons (get-minmax actual) (car puntuaciones))
                         (cdr puntuaciones));se añade puntuación actual a la puntuación del nivel
                   (cons (list (get-tab actual) (get-minmax actual) (get-minmax actual)) (list arbol));se añade nodo actual al arbol
                   )
          (if (= (get-tipo actual) -1) ;no puntuado
              (plantar (car abiertos);actual'
                       (cdr abiertos);abiertos'
                       ;calcular puntuación del nodo en función de sus hijos, y añadir
                       ;esa puntuación a la lista de puntuaciones del nivel del nodo
                       (cons (cons (min-l (car puntuaciones)) (cadr puntuaciones))                                        ;en vez de min hay que hacer minmax
                             (cddr puntuaciones))
                       (cons (list (get-tab actual) (get-minmax actual) (min-l (car puntuaciones))) arbol)) ;se añade el nodo puntuado
              ;si no, tiene que ser no explotado (tipo = -2)
              ((lambda (x) (plantar (car x);actual'
                       (cdr x) ;se añaden hijos menos el primero para busqueda en profundidad
                       (cons '() puntuaciones) ;se añade nivel al arbol de puntuaciones
                       arbol ;no se ha puntuado ningun nodo, se pasa igual
                       ))(append (explotar actual) abiertos))
              )
          )
      )
  )
                       
(define test (plantar '((1 2) 1 -2 -1) '(((2 1) 1 -2 -1) ((2 2) 0 -1 -1)) '(()) '()))

                   
      
      
