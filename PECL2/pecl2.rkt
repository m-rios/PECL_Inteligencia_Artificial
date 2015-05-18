#lang racket
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
;calcula los hijos de un nodo del árbol de búsqueda
(define (explotar nodo)
  (append (explotar-izq nodo '() (get-minmax nodo)) ;hijos jugando por la izquierda
          (explotar-der nodo '() (get-minmax nodo)) ;hijos jugando por la derecha
          (list (list (get-tab nodo) (get-minmax nodo) -1 (get-pun nodo)));mismo nodo, para hacer backtracking
          ))
(define (explotar-izq nodo hijos minmax)
  (if (= 1 (first (get-tab nodo)))
      hijos
      ((lambda (x) (explotar-izq x (append hijos (list x)) minmax)) 
       (list 
        (list (- (first (get-tab nodo)) 1) (second (get-tab nodo)));tab
        (remainder (+ 1 minmax) 2); min-max
        -2 ;no explotado
        -1 ;no tiene puntuacion
        ))
      ))

(define (explotar-der nodo hijos minmax)
  (if (= 1 (second (get-tab nodo)))
      hijos
      ((lambda (x) (explotar-der x (append hijos (list x)) minmax)) 
       (list 
        (list (first (get-tab nodo)) (- (second (get-tab nodo)) 1));tab
        (remainder (+ 1 minmax) 2); min-max
        -2 ;no explotado
        -1 ;no tiene puntuacion
        ))
      ))  
;obtiene la puntuación de un nodo intermedio
(define (puntuar nodo puntuaciones)
  (if (= 0 (get-minmax nodo))
      (max-l puntuaciones);si max, devuelve el máximo de los hijos
      (min-l puntuaciones);si min, devuelve el mínimo de los hijos
      ))
;crea el árbol de decisiones  
(define (plantar-aux actual abiertos puntuaciones arbol)
  (if (and (empty? abiertos) (not(empty? arbol))) ;si abiertos vacio y arbol no vacio, casi se ha terminado el árbol
      (cons 
       (list (get-tab actual) (get-minmax actual) (get-tipo actual) (puntuar actual (car puntuaciones))) arbol)
      (if (equal? '(1 1) (get-tab actual))
          (plantar-aux (car abiertos);actual'
                   (cdr abiertos);abiertos'
                   (cons (cons (get-minmax actual) (car puntuaciones))
                         (cdr puntuaciones));se añade puntuación actual a la puntuación del nivel
                   (cons (list (get-tab actual) (get-minmax actual) (get-tipo actual) (get-minmax actual)) arbol);se añade nodo actual al arbol
                   )
          (if (= (get-tipo actual) -1) ;no puntuado
              (plantar-aux (car abiertos);actual'
                       (cdr abiertos);abiertos'
                       ;calcular puntuación del nodo en función de sus hijos, y añadir
                       ;esa puntuación a la lista de puntuaciones del nivel del nodo
                       (cons (cons (puntuar actual (car puntuaciones)) (cadr puntuaciones))
                             (cddr puntuaciones))
                       (cons (list (get-tab actual) (get-minmax actual) (get-tipo actual) (min-l (car puntuaciones))) arbol)) ;se añade el nodo puntuado
              ;si no, tiene que ser no explotado (tipo = -2)
              ((lambda (x) (plantar-aux (car x);actual'
                       (cdr x) ;se añaden hijos menos el primero para busqueda en profundidad
                       (cons '() puntuaciones) ;se añade nivel al arbol de puntuaciones
                       arbol ;no se ha puntuado ningun nodo, se pasa igual
                       ))(append (explotar actual) abiertos))
              )
          )
      )
  )
;inicializa los parámetros para el algoritmo
;al borrar los repetidos, devuelve los distintos nodos que pueden aparecer en
;el arbol, con su valor (ganar/perder) asociado, para apoyar a la toma
;de decisiones por parte de la máquina
(define (plantar n) 
  (if (= n 0)
      "no tiene sentido jugar si no hay divisiones"
      (remove-duplicates (plantar-aux (list (dinA n) 0 -2 -1) '() '() '())))
  )
;inicializa los parámetros para jugar-aux
(define (jugar n)
  (jugar-aux true (list (dinA n) 0 -2 -1) (plantar n)))
;algoritmo que implementa las jugadas
(define (jugar-aux maquina nodo arbol)
  (if (equal? (first nodo) '(1 1))
      (if maquina "tu ganas" "perdiste!")
      (if maquina
          (jugar-aux (not maquina) (jugada (explotar nodo) arbol) arbol)
          false
          )))
;jugada de máquina
(define (jugada opciones arbol)
  false)