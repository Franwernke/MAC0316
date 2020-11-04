#lang scheme

;Sum of all elements in the list [m, m + 1, m + 2, ..., m]
(define (sigma m n)
    (if (= m n)
        n
        (+ m (sigma (+ m 1) n))
    )
)

;Find m to the n-th power
(define (exp m n)
    (if (= n 0)
        1
        (* m (exp m (- n 1)))
    )
)

;Find the ceil(log_m(n))
(define log
    (lambda (m n)
        (if (> m n)
            0
            (+ 1 (log m (/ n m)))
        )
    )
)

;Find the number of combination of k elements in a total of n
(define choose
    (lambda (n k)
        (if (or (= k n) (= k 0))
            1
            (+ (choose (- n 1) k)
               (choose (- n 1) (- k 1))
            )
        )
    )
)

;Find the n-th fibonacci number
(define fib
    (lambda (n)
        n
        (+ (fib (- n 1))
           (fib (- n 2))
        )
    )
)

;Return a list with all numbers from m to n
(define interval
    (lambda (m n)
        (if (> m n)
            '()
            (cons m (interval (+ m 1) n))
        )
    )
)

;Removes all multiples of k in a list
(define removeMult
    (lambda (list k)
        (if (null? list)
            '()
            (if (= (modulo (car list) k) 0)
                (removeMult (cdr list) k)
                (cons (car list) (removeMult (cdr list) k))
            )
        )
    )
)

;Returns a list where all the elements are coprimes
(define filtro
    (lambda (l)
        (if (null? l)
            '()
            (cons (car l) (filtro (removeMult (cdr l) (car l)))
            )
        )
    )
)

;Returns if a element is in a list
(define find
    (lambda (x l)
        (if (null? l)
            #f
            (if (equal? (car l) x)
                #t
                (find x (cdr l))
            )
        )

    )
)

;Returns if a number is prime
(define prime
    (lambda (n)
        (if (find n (filtro (interval 2 n)))
            #t
            #f
        )
    )
)

;Returns the sum of the n first primes after m
(define sumPrime
    (lambda (m n)
        (if (= 0 n)
            0
            (if (prime m)
                (+ m (sumPrime (+ m 1) (- n 1)))
                (sumPrime (+ m 1) n)
            )
        )
    )
)