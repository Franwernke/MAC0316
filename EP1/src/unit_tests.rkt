#lang plai-typed

; testando let
(test (interpS '(let ([x 10]) (+ x x)))
      (numV 20))

; testando let*
(test (interpS '(let* ([x 10]  [y 20]) (+ x y)))
      (numV 30))

; testando letrec
(test (interpS '(letrec ([fac (lambda n (if n (* n (call fac (- n 1))) 1))])
                  (call fac 5)))
      (numV 120))

; testando quote
(test (interpS '(quote oi))
      (symV 'oi))

; testando load
(interpS '(load))
; aqui seu programa deve exibir os resultados das expressões recebidas
; na caixa do read, como por exemplo:
; IN: (+ 2 3)
; OUT: (numV 5)
