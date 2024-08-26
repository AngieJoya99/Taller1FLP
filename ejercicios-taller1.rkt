#|Angie Joya - 2322609
Emily Nuñez - 2240156|#

#lang eopl

#|Punto 1
invert: List x predicado -> List
usage: (invert L P) = Lista con pares invertidos (y,x) que cumplen el predicado P

Gramática:

Casos de prueba:
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
(invert '((6 9) (10 90) (82 7) ) odd?)
(invert '((10 9) (0 0) (15 15) (0 0) (8 8) (15 1048) (24 24) ) zero?)
(invert '((10 8) (-15 8) (-2 -8) (3 -14) (-40 -25) ) negative?)
|#

(define invert
    (lambda (L P)
        (cond
            [(null? L) L]
            [
                (and (P (caar L)) (P (cadr (car L)))) 
                (cons (cons (cadr (car L)) (cons (caar L) '())) (invert (cdr L) P))
            ]            
            [else (invert (cdr L) P)]
        )
    )
)

#|---------------------------------------------------------------------------------
Punto 2
down: List -> List
usage: (down L) = Lista con cada elemento en un nivel más de paréntesis

Casos de prueba:
(down '(1 2 3))
(down '((una) (buena) (idea)))
(down '(un (objeto (mas)) complicado))
|#
(define down
    (lambda (L)
        (cond
            [(null? L) L]
            [else (append (list(list (car L))) (down (cdr L)))]
        ) 
    )
)

#|---------------------------------------------------------------------------------
Punto 3
list-set: List x Int x List x predicado -> List
usage: (list-set L n x P) = Lista con el elemento x añadido en la posición n de L
el elemento original en esta posición cumple el predicado P

Casos de prueba:
(list-set '(5 8 7 6) 2 '(1 2) odd?)
(list-set '(5 8 7 6) 2 '(1 2) even?)
|#
(define list-set
    (lambda (L n x P)
        (letrec
            ([contador
                (lambda (L n x P i)
                    (cond
                        [(null? L) L]
                        [(and(eq? n i) (P (car L)))
                            (append (list x) (contador (cdr L) n x P (+ i 1)))
                        ]
                        [else (append (list (car L)) (contador (cdr L) n x P (+ i 1)))]
                    )                
                )
            ])
            (contador L n x P 0)
        )  
    )
)

#|---------------------------------------------------------------------------------
Punto 4
filter-in: predicado x List -> List
usage: (filter-in P L) = Lista con los elementos de L que cumplen el predicado P

Casos de prueba:
(filter-in number? '(a 2 (1 3) b 7))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))
|#
(define filter-in
    (lambda (P L)
        (cond
            [(null? L) empty]
            [(P (car L)) 
                (append (list (car L)) (filter-in P (cdr L)))
            ]
            [else (filter-in P (cdr L))]
        )   
    )
)

#|---------------------------------------------------------------------------------
Punto 5
list-index: predicado x List -> SchemeVal
usage: (list-index P L) = posición del primer elemento de L que cumple con P, o #f si no hay ningún
elemento que lo cumpla

Casos de prueba: 
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))
|#
(define list-index
    (lambda (P L)
        (letrec
            ([contador
                (lambda (P L n)
                    (cond
                        [(null? L) #f]
                        [(P (car L)) (cons n '())]
                        [else (contador P (cdr L)(+ n 1))]
                    )                
                )
            ])
            (contador P L 0)
        )
    )
)

(define contador
    (lambda (P L n)
        (cond
            [(null? L) #f]
            [(P (car L)) (cons n '())]
            [else (contador P (cdr L)(+ n 1))]
        )
    
    )
)


#|---------------------------------------------------------------------------------
Punto 6
swapper: SchemeVal x SchemeVal x List -> List
usage: (swapper E1 E2 L) = Lista con las instancias de E1 reemplazadas por E2

Casos de prueba: 
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))
|#
(define swapper
    (lambda (E1 E2 L)
        (cond
            [(null? L) L]
            [(eqv? E1 (car L)) 
                (append (list E2) (swapper E1 E2 (cdr L)))
            ]
            [(eqv? E2 (car L)) 
                (append (list E1) (swapper E1 E2 (cdr L)))
            ]
            [else (append (list (car L)) (swapper E1 E2 (cdr L)))]
        )    
    )
)

#|---------------------------------------------------------------------------------
Punto 7
cartesian-product: List x List -> List
usage: (cartesian-product L1 L2) = Lista de tuplas con el producto cartesiano entre L1 y L2

Casos de prueba:
(cartesian-product '(a b c) '(x y))
|#
(define cartesian-product
    (lambda (L1 L2)
        (letrec
            ([recorrerL2
                (lambda (x L2)
                    (cond
                        [(null? L2) L2]
                        [else (cons (list x (car L2)) (recorrerL2 x (cdr L2)))]
                    )                                   
                )
            ]
            [recorrerL1
                (lambda (L1 L2)
                    (cond
                        [(null? L1) L1]
                        [else (append (recorrerL2 (car L1) L2) (recorrerL1 (cdr L1) L2))]
                    )                
                )
            ])
            (recorrerL1 L1 L2)
        )
    )
)

#|---------------------------------------------------------------------------------
Punto 8
mapping: función x List x List -> List
usage: (mapping F L1 L2) = Lista de pares (a,b) siendo a elemento de L1 y b elemento de L2, 
para los cuales evaluar a en F retorna b

Casos de prueba: 
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
((1 2) (2 4) (3 6))
|#
(define mapping
    (lambda (F L1 L2)
        (letrec
            ([recorrerL2
                (lambda (x L2)
                    (cond
                        [(null? L2) L2]
                        [else (if (equal? (F x) (car L2))
                            (list (list x (car L2)))  ; Si F(x) es igual a (car L2), devuelve el par (x (car L2)) como una lista
                            (recorrerL2 x (cdr L2))
                        )]
                    )                                   
                )
            ]
            [recorrerL1
                (lambda (L1 L2)
                    (cond
                        [(null? L1) L1]
                        [else (append (recorrerL2 (car L1) L2) (recorrerL1 (cdr L1) L2))]
                    )                
                )
            ])
            (recorrerL1 L1 L2)
        )    
    )
)

#|---------------------------------------------------------------------------------
Punto 9
inversions: List -> Int
usage: (inversions L) = número de Inversiones de L. Se dice una pareja (a1 a2) es inversión si la
posición en la lista de a1 es menor a la de a2 y a1 es mayor que a2
|#
(define inversions
    (lambda (L)
        (display "Prueba")    
    )
)

#|---------------------------------------------------------------------------------
Punto 10
up: List -> Lista
usage: (up L) = Lista con cada elemento en un nivel menos de paréntesis. Si el elemento no tiene
paréntesis, no se le hacen modificaciones

Casos de prueba:
(up '((1 2) (3 4)))
(up '((x (y)) z))
|#
(define up
    (lambda (L)
        (cond
            [(null? L) L]
            [(list? (car L))
                (append (car L) (up(cdr L)))
            ]
            [else (cons (car L) (up(cdr L)))]
        )    
    )
)

#|---------------------------------------------------------------------------------
Punto 11
zip: función x List x List -> List
usage: (zip F L1 L2) = Lista donde la cada posición i es el resultado de evaluar F con el elemento i
de L1 y el elemento i de L2

Casos de prueba:
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))
|#
(define zip
    (lambda (F L1 L2)
        (cond
            [(null? L1) L1]
            [(null? L2) L2]
            [else (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))]
        )    
    )
)

#|---------------------------------------------------------------------------------
Punto 12
filter-acum: Int x Int x función x Int x predicado -> Int
usage: (filter-acum a b F acum filter) = aplicar F a todos los enteros entre a y b (cerrado)
que cumplen con el predicado filter, el resultado se acumula en acum y se retorna al final
|#
(define filter-acum
    (lambda (a b F acum filter)
        (display "Prueba")    
    )
)

#|---------------------------------------------------------------------------------
Punto 13
operate: List x List -> Int
usage: (operate lrators lrands) = Resultado de aplicar sucesivamente las operaciones 
en lrators a los valores en lrands
|#
(define operate
    (lambda (lrators lrands)
        (display "Prueba")    
    )
)

#|---------------------------------------------------------------------------------
Punto 14
path: Int x arbol-binario -> List
usage: (path n BST) = Lista con la ruta para llegar de la raíz al número n indicado por las
cadenas de texto left y right. Si el número no se encuentra, retorna una lista vacía
|#
(define path
    (lambda (n BST)
        (display "Prueba")    
    )
)

#|---------------------------------------------------------------------------------
Punto 15
count-odd-and-even: arbol-binario -> List
usage: (count-odd-and-even arbol) = Lista con dos elementos, el primero indica la cantidad de
números pares en el árbol y el segundo la cantidad de impares
|#
(define count-odd-and-even
    (lambda (arbol)
        (display "Prueba")    
    )
)

#|---------------------------------------------------------------------------------
Punto 16
simpson-rule: función x Int x Int x Int -> Int
usage: (simpson-rule f a b n) = Calcula el resultado de la integral de f entre a y b para un entero par n
|#
(define simpson-rule
    (lambda (f a b n)
        (display "Prueba")    
    )
)

#|---------------------------------------------------------------------------------
Punto 17
prod-scalar-matriz: List x List -> List
usage: (prod-scalar-matriz mat vec) = Calcula la multiplicación entre la matriz mat y el vector vec,
el resultado es representado por una lista
|#
(define prod-scalar-matriz
    (lambda (mat vec)
        (display "Prueba")    
    )
)

#|---------------------------------------------------------------------------------
Punto 18
pascal: Int -> List
usage: (pascal N) = Calcula la fila N del triángulo de pascal, el resultado se representa por una lista
|#
(define pascal
    (lambda (N)
        (display "Prueba")    
    )
)