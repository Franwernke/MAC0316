#lang scheme

;Counts all occurrencies of x in l
(define count
    (lambda (x l)
        (if (null? l)
            0
            (if (equal? (car l) x)
                (+ 1 (count x (cdr l)))
                (count x (cdr l))
            )
        )
    )
)

;Counts all occurencies of x in l and sublists of l
(define countall
    (lambda (x l)
        (if (null? l)
            0
            (if (list? (car l))
                (countall x (car l))
                (if (equal? x (car l))
                    (+ 1 (countall x (cdr l)))
                    (countall x (cdr l))
                )
            )
        )
    )
)

;Reverse a list
(define reverse
    (lambda (l)
        (if (null? l)
            '()
            (append (reverse (cdr l)) (list (car l)))
        )
    )
)

;Reverse a list and it's sublists
(define twist
    (lambda (l)
        (if (null? l)
            '()
            (if (list? (car l))
                (cons (twist (cdr l)) (twist (car l)))
                (append (twist (cdr l)) (list (car l)))
            )
        )
    )
)

;Returns if a element is an atom
(define atom?
    (lambda (x)
        (if (list? x)
            #f
            #t
        )
    )
)

;Returns all atoms in a list
(define flatten
    (lambda (l)
        (if (null? l)
            '()
            (if (atom? (car l))
                (cons (car l) (flatten (cdr l)))
                (append (flatten (car l)) (flatten (cdr l)))
            )
        )
    )
)