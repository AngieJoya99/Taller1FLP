#|Angie Joya - 2322609
Emily Nuñez - 2240156|#

#lang eopl

#|Documentar procedimientos, gramáticas, funciones auxiliares, casos de prueba (min 2)|#

#|Punto 1
invert: List x predicado -> List
usage: (invert lista p) = Lista con pares invertidos (y,x) que cumplen el predicado p 

<lista>:= ()
          ((<entero> <entero>) <lista>)|#

(define invert
    (lambda (lista p)
        (cond
            [(null? lista) lista]

            [(null? (cdr lista)) 
                (if (and (p (caar lista)) (p (cadr (car lista)))) 
                    (cons (cons (cadr (car lista)) (cons (caar lista) '())) '())
                    lista
                )                
            ]

            [(not(null? (cdr lista)))
                (if (and (p (caar lista)) (p (cadr (car lista)))) 
                    (cons (cons (cadr (car lista)) (cons (caar lista) '())) (invert (cdr lista) p))
                    (cons '() (invert (cdr lista) p))
                )                
            ]             
        )
    )
)

(invert '((3 2) (4 2) (1 5) (2 8)) even?)


