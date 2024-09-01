#|Angie Joya - 2322609
Emily Nuñez - 2240156|#

#lang eopl

#|Función Auxiliar
juntarListas: List x List -> List
usage: (juntarListas L1 L2) = Lista resultante de juntar los elementos de L1 con los elementos de L2

Gramática: <lista> := () | (<valor-de-scheme> <lista>)
|#
(define juntarListas
    (lambda (L1 L2)
        (cond
            [(null? L1) L2]
            [else (cons (car L1) (juntarListas (cdr L1) L2))]
        )
    )
)

#|---------------------------------------------------------------------------------
Punto 1
invert: List x predicado -> List
usage: (invert L P) = Lista con pares invertidos (y,x) que cumplen el predicado P

Gramática: <lista> := () | ((<int> <int>) <lista>)

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

Gramática: <lista> := () | (<valor-de-scheme> <lista>)

Casos de prueba:
(down '(8 45 20 (10 8) 5))
(down '())
(down '((caso) (de) (Prueba)))
(down '(un (objeto (mas)) complicado))
|#
(define down
    (lambda (L)
        (cond
            [(null? L) L]
            [else (juntarListas (list(list (car L))) (down (cdr L)))]
        ) 
    )
)

#|---------------------------------------------------------------------------------
Punto 3
list-set: List x Int x List x predicado -> List
usage: (list-set L n x P) = Lista con el elemento x añadido en la posición n de L
el elemento original en esta posición cumple el predicado P

Gramática: <lista> := () | (<valor-de-scheme> <lista>)

Casos de prueba:
(list-set '(1 2 3 4 5) 2 '(8 30) odd?)
(list-set '(4 0 2 9 6) 1 '(9) zero?)
(list-set '(5 -2 8 -7 -25 -96) 0 '(9) negative?)
|#
(define list-set
    (lambda (L n x P)
        (letrec
            ([contador
                (lambda (L n x P i)
                    (cond
                        [(null? L) L]
                        [(and(eq? n i) (P (car L)))
                            (juntarListas (list x) (contador (cdr L) n x P (+ i 1)))
                        ]
                        [else (juntarListas (list (car L)) (contador (cdr L) n x P (+ i 1)))]
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

Gramática: <lista> := () | (<valor-de-scheme> <lista>)

Casos de prueba:
(filter-in number? '(z u 15 (1 2 3) 8 10 k))
(filter-in negative? '(1 -5 2 -8 -9 -14 5 0 1))
(filter-in zero? '(1 2 3 4 5))
(filter-in string? '(a b c "caso" "de" 5 "prueba" (1 3)))
|#
(define filter-in
    (lambda (P L)
        (cond
            [(null? L) empty]
            [(P (car L)) 
                (juntarListas (list (car L)) (filter-in P (cdr L)))
            ]
            [else (filter-in P (cdr L))]
        )   
    )
)

#|---------------------------------------------------------------------------------
Punto 5
list-index: predicado x List -> int | #f
usage: (list-index P L) = posición del primer elemento de L que cumple con P, o #f si no hay ningún
elemento que lo cumpla

Gramática: <lista> := () | (<valor-de-scheme> <lista>)

Casos de prueba: 
(list-index number? '(a x 5 6 (1 2 3) h -7))
(list-index number? '(a b c d e))
(list-index symbol? '(a (b c) @ foo))
(list-index negative? '(0 1 4 5 -9 8 -4))
|#
(define list-index
    (lambda (P L)
        (letrec
            ([contador
                (lambda (P L n)
                    (cond
                        [(null? L) #f]
                        [(P (car L)) n]
                        [else (contador P (cdr L)(+ n 1))]
                    )                
                )
            ])
            (contador P L 0)
        )
    )
)

#|---------------------------------------------------------------------------------
Punto 6
swapper: SchemeVal x SchemeVal x List -> List
usage: (swapper E1 E2 L) = Lista con las instancias de E1 reemplazadas por E2
y las instancias de E2 reemplazadas por E1

Gramática: <lista> := () | (<valor-de-scheme> <lista>)

Casos de prueba: 
(swapper 'a 'z '(a b c z))
(swapper 'a '@ '(n k a g h a l i (a b)))
(swapper 0 1 '(0 1 0 1 1 0 0 1))
|#
(define swapper
    (lambda (E1 E2 L)
        (cond
            [(null? L) L]
            [(eqv? E1 (car L)) 
                (juntarListas (list E2) (swapper E1 E2 (cdr L)))
            ]
            [(eqv? E2 (car L)) 
                (juntarListas (list E1) (swapper E1 E2 (cdr L)))
            ]
            [else (juntarListas (list (car L)) (swapper E1 E2 (cdr L)))]
        )    
    )
)

#|---------------------------------------------------------------------------------
Punto 7
cartesian-product: List x List -> List
usage: (cartesian-product L1 L2) = Lista de tuplas con el producto cartesiano entre L1 y L2

Gramática: <lista> := () | (<valor-de-scheme> <lista>)

Casos de prueba:
(cartesian-product '(a b c) '(1 2 3))
(cartesian-product '(0 1 0) '(1 0 1))
(cartesian-product '(z y x) '(@ 1 "prueba"))
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
                        [else (juntarListas (recorrerL2 (car L1) L2) (recorrerL1 (cdr L1) L2))]
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

Gramática: <lista> := () | (<int> <lista>)

Casos de prueba: 
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
(mapping (lambda (x) (+ x 1)) '(1 2 3) '(2 4 5))
(mapping (lambda (x) (* x 2)) '(1 2 3) '(5 7 9))
(mapping (lambda (x) x) '(1 2 3) '(3 2 1))
|#
(define mapping
    (lambda (F L1 L2)
        (letrec
            ([recorrerL2
                (lambda (x L2)
                    (cond
                        [(null? L2) L2]
                        [else (if (equal? (F x) (car L2))
                            (list (list x (car L2)))
                            (recorrerL2 x (cdr L2))
                        )]
                    )                                   
                )
            ]
            [recorrerL1
                (lambda (L1 L2)
                    (cond
                        [(null? L1) L1]
                        [else (juntarListas (recorrerL2 (car L1) L2) (recorrerL1 (cdr L1) L2))]
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

Gramática: <lista> := () | (<int> <lista>)

Casos de prueba: 
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))
(inversions '(3 2 1))
|#
(define inversions
    (lambda (L)
        #|
          aux : Int x List x Int -> Int
          usage: Función que recorre una lista y cuenta cuántos elementos son menores que e, 
          incrementando el contador cont en cada caso. Devuelve el contador final cuando recibe una lista vacía.
        |#
        (define aux
            (lambda (e L2 cont)
                (cond 
                    [(null? L2) cont]
                    [(> e (car L2)) (aux e (cdr L2) (+ cont 1))]
                    [else (aux e (cdr L2) cont)]
                )
            )
        )
        (if (null? L)
            0
            (+ (aux (car L) (cdr L) 0) (inversions (cdr L))) 
        )    
    )
)
#|---------------------------------------------------------------------------------
Punto 10
up: List -> Lista
usage: (up L) = Lista con cada elemento en un nivel menos de paréntesis. Si el elemento no tiene
paréntesis, no se le hacen modificaciones

Gramática: <lista> := () | (<valor-de-scheme> <lista>)

Casos de prueba:
(up '((1 2) (3 4) (5 6)))
(up '((a (b)) c))
(up '())
(up '(1 2 3 4 5))
|#
(define up
    (lambda (L)
        (cond
            [(null? L) L]
            [(list? (car L))
                (juntarListas (car L) (up(cdr L)))
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

Gramática: <lista> := () | (<int> <lista>)

Casos de prueba:
(zip + '(2 4) '(6 8))
(zip * '(8 4 9) '(5 6 9))
(zip / '(10 80 9 0) '(5 4 3 500))
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

Gramática: <lambda-exp> ::= <identificador>
::= (lambda (<identificador>) <lambda-exp>)
::= (<lambda-exp> <lambda-exp>)

Casos de prueba:
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)
(filter-acum 1 10 * 1 even?)
(filter-acum 1 10 * 1 odd?)
|#
(define filter-acum
    (lambda (a b F acum filter)
        (cond
            [(> a b) acum ]
            [(filter a) (filter-acum (+ a 1) b F (F a acum) filter)]
            [else (filter-acum (+ a 1) b F acum filter)]
        )    
    )
)

#|---------------------------------------------------------------------------------
Punto 13
operate: List x List -> Int
usage: (operate lrators lrands) = Resultado de aplicar sucesivamente las operaciones 
en lrators a los valores en lrands

Gramática: <lista> := () | (<int> <lista>)

Casos de prueba: 
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))
|#
(define operate
    (lambda (lrators lrands)
        #|
          aux: List x List x Int -> Int
          usage: Recorre las listas simultáneamente. En cada paso, aplica la función contenida en la 
          cabeza de lrat2 al acumulador y al primer elemento de lran2. Cuando la primera lista (lrat2)
          se vacía, devuelve el valor acumulado.
        |#
        (define aux
            (lambda (lrat2 lran2 acumulador)
                (if (null? lrat2) 
                    acumulador
                    (aux (cdr lrat2) (cdr lran2) ((car lrat2) acumulador (car lran2)))
                )
            )
        )
        (aux lrators (cdr lrands) (car lrands))
    )
)

#|---------------------------------------------------------------------------------
Punto 14
path: Int x arbol-binario -> List
usage: (path n BST) = Lista con la ruta para llegar de la raíz al número n indicado por las
cadenas de texto left y right. Si el número es encontrado en el nodo raiz, retorna una lista vacía
Casos de prueba:

Gramática: <arbol-binario> := (arbol-vacio) empty
:= (nodo) numero <arbol-binario> <arbol-binario>

Casos de prueba:
(path 17 '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
(path 7 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
(path 18 '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
(path 10 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
|#
(define path
    (lambda (n BST)
        (cond
            [(null? BST) '()]
            [(= n (car BST)) '()]
            [(< n (car BST)) (cons 'left (path n (cadr BST)))]
            [(> n (car BST)) (cons 'right (path n (caddr BST)))]
            [else '()]
        )
    )
)

#|---------------------------------------------------------------------------------
Punto 15
count-odd-and-even: arbol-binario -> List
usage: (count-odd-and-even arbol) = Lista con dos elementos, el primero indica la cantidad de
números pares en el árbol y el segundo la cantidad de impares

Gramática: <arbol-binario> := (arbol-vacio) empty
:= (nodo) numero <arbol-binario> <arbol-binario>

Casos de prueba:
(count-odd-and-even '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
(count-odd-and-even '(22 (11 () (18 () (2 () (7 () ())))) (44 (30 (13 () (17 () ())) ()) (27 () ()))))
(count-odd-and-even '(22 (11 () (18 () (2 () (7 () ())))) (44 (30 (13 () ()) ()) (27 () ()))))
(count-odd-and-even '(19 (25 () (22 () ())) (35 (28 (14 () ()) ()) (9 () ()))))
|#
(define count-odd-and-even
    (lambda (arbol)
        #|
          cantidad-int: arbol-binario x Int x Int -> List
          usage: recorre un árbol binario y cuenta cuántos números enteros en el árbol son pares
          y cuántos son impares. Devuelve una lista donde el primer elemento es la cantidad de 
          números pares y el segundo es la cantidad de números impares.
        |#
        (define cantidad-int 
            (lambda (arbol even odd)
                (cond
                    [(number? arbol)
                        (if (even? arbol)
                        (list (+ even 1) odd)
                        (list even (+ odd 1)))]
                    [(list? arbol)
                        #|
                          aux: arbol-binario x Int x Int -> List
                          usage: Recorre recursivamente una lista (que representa un árbol binario) para 
                          contar cuántos números son pares y cuántos son impares. La función acumula 
                          estos conteos y los devuelve como una lista de dos elementos
                        |#
                        (define aux 
                            (lambda (arbol2 even odd)
                                (if (null? arbol2)
                                    (list even odd)
                                    (letrec
                                        (
                                            (arbol-num (car arbol2))
                                            (contador (cantidad-int arbol-num even odd))
                                        )
                                        (aux (cdr arbol2) (car contador) (cadr contador))
                                    )
                                )
                            )
                        )
                        (aux arbol even odd)
                    ]
                    [else (list even odd)]
                )
            )
        )
        (cantidad-int arbol 0 0)
    )
)


#|---------------------------------------------------------------------------------
Punto 16
simpson-rule: función x Int x Int x Int -> Int
usage: (simpson-rule f a b n) = Calcula el resultado de la integral de f entre a y b para un entero par n

Gramática: <lambda-exp> ::= <identificador>
::= (lambda (<identificador>) <lambda-exp>)
::= (<lambda-exp> <lambda-exp>)

Casos de prueba:
(simpson-rule (lambda (x) (* x (* x x))) 1 5 8)
(simpson-rule (lambda (x) x) 1 5 12)
|#
(define simpson-rule
    (lambda (f a b n)
        (letrec
            (
                (h  (/ (- b a) n))
                (aux
                    (lambda (f2 a2 n2 k acum)
                        (cond
                            [(= k 0)(aux f2 a n2 (+ k 1) (+ acum (f2 (+ a2 (* k h )))))]
                            [(> k n2) acum]
                            [(= k n2)(aux f2 a n2 (+ k 1) (+ acum (f2 (+ a2 (* k h )))))]
                            [(even? k) (aux f2 a n2 (+ k 1) (+ acum (* 2 (f2 (+ a2 (* k h ))))))]
                            [(odd? k) (aux f2 a n2 (+ k 1) (+ acum (* 4 (f2 (+ a2 (* k h ))))))]
                            [else acum]
                        )
                    )
                )
            )
            (* (/ h 3) (aux f a n 0 0))
        )
    )
)

#|---------------------------------------------------------------------------------
Punto 17
prod-scalar-matriz: List x List -> List
usage: (prod-scalar-matriz mat vec) = Calcula la multiplicación entre la matriz mat y el vector vec,
el resultado es representado por una lista

Gramática: <lista> := () | (<int> <lista>)

Casos de prueba:
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
(prod-scalar-matriz '((1 2 4) (2 4 7) (4 5 7)) '(2 4 5))
(prod-scalar-matriz '((1 2 4 5) (2 4 7 8) (4 5 7 10)) '(2 4 5 10))
|#
(define prod-scalar-matriz
    (lambda (mat vec)
        #|
          aux: List x List -> List
          usage:  Recorre ambas listas simultáneamente, multiplicando los elementos correspondientes 
          de L1 y L2. Construye una nueva lista con los productos de estos pares utilizando cons.
          Posteriormente, cuando la lista L1 se vacia, la funcione retorna una lista vacía.
        |#
        (define aux
            (lambda (L1 L2)
                (if (null? L1)
                    '()
                    (cons (* (car L1) (car L2))(aux (cdr L1) (cdr L2)))
                )
            )
        )
        (if (null? mat)
            '()
            (cons (aux (car mat) vec)(prod-scalar-matriz (cdr mat) vec))
        )
    )
)

#|---------------------------------------------------------------------------------
Punto 18
pascal: Int -> List
usage: (pascal N) = Calcula la fila N del triángulo de pascal, el resultado se representa por una lista

Gramática: <lista> := () | (<int> <lista>)

Casos de prueba:
(pascal 5)
(pascal 1)
(pascal 2)
(pascal 10)

|#
(define pascal
    (lambda (N)
        (letrec
            ([sumarLista
                (lambda (L)
                    (cond
                        [(null? (cdr L)) L]
                        [else (juntarListas (list (+ (car L)(cadr L))) (sumarLista (cdr L)))]
                    )
                )
            ]
            [generarLista
                (lambda (i L)
                    (cond
                        [(equal? i N) (cons 1 L)]
                        [else (generarLista(+ i 1) (sumarLista (cons 1 L)))]
                    )
                )
            ])
            (generarLista 1 '())
        )
    )
)