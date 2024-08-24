#|Angie Joya - 2322609
Emily Nuñez - 2240156|#

#lang eopl

#|Documentar procedimientos, gramáticas, funciones auxiliares, casos de prueba (min 2)|#

#|Punto 1
invert: List x predicado -> List
usage: (invert lista p) = Lista con pares invertidos (y,x) que cumplen el predicado p 
|#

(define invert
    (lambda (lista p)
        (cond
            [(null? lista) lista]

            [(null? (cdr lista)) 
                (if (and (p (caar lista)) (p (cadr (car lista)))) 
                    (cons (cons (cadr (car lista)) (cons (caar lista) '())) '())
                    '()
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

#|Casos de prueba:
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
|#


#|---------------------------------------------------------------------------------
Punto 2
down: List -> List
usage: (down lista) = Lista con cada elemento en un nivel más de paréntesis
|#
