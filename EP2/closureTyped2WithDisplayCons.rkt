#lang plai-typed

; Aluno: Francisco Eugênio Wernke
; NUSP: 11221870

; Basic expressions
(define-type ExprC
  [numC  (n : number)]
  [idC   (s : symbol)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC  (arg : symbol) (body : ExprC)]
  [appC  (fun : ExprC) (arg : ExprC)]
  [ifC   (c : ExprC) (y : ExprC) (n : ExprC)]
  [seqC  (e1 : ExprC) (e2 : ExprC)]
  [letC  (name : symbol) (arg : ExprC) (body : ExprC)]
  [let*C (name1 : symbol) (arg1 : ExprC) (name2 : symbol) (arg2 : ExprC) (body : ExprC)]
  [letrecC (name : symbol) (arg : ExprC) (body : ExprC)]
  [consC (car : ExprC) (cdr : ExprC)]
  [carC  (cell : ExprC) ]
  [cdrC (cell : ExprC)]
  [displayC (exp : ExprC)]
  [quoteC  (sym : symbol)]
  [equal?C (val1 : ExprC) (val2 : ExprC)]
  [nullC  ]
  )


; Sugared expressions
(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)]
  [lamS    (arg : symbol) (body : ExprS)]
  [appS    (fun : ExprS) (arg : ExprS)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [seqS    (e1 : ExprS) (e2 : ExprS)]
  [letS    (name : symbol) (arg : ExprS) (body : ExprS)]
  [let*S   (name1 : symbol) (arg1 : ExprS) (name2 : symbol) (arg2 : ExprS) (body : ExprS)]
  [letrecS (name : symbol) (arg : ExprS) (body : ExprS)]
  [consS (car : ExprS) (cdr : ExprS)]
  [carS (cell : ExprS) ]
  [cdrS (cell : ExprS)]
  [displayS (exp : ExprS)]
  [quoteS  (sym : symbol)]
  [equal?S (val1 : ExprS) (val2 : ExprS)]
  [nullS ]
 )


; Removing the sugar
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [lamS    (a b)      (lamC a (desugar b))]
    [appS    (fun arg)  (appC (desugar fun) (desugar arg))]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c s n)    (ifC (desugar c) (desugar s) (desugar n))]
    [seqS    (e1 e2)    (seqC (desugar e1) (desugar e2))]
    [letS    (n a b)    (letC n (desugar a) (desugar b))]
    [let*S   (name1 arg1 name2 arg2 body) (let*C name1 (desugar arg1) name2 (desugar arg2) (desugar body))]
    [letrecS (name1 arg1 body) (letrecC name1 (desugar arg1) (desugar body))]
    [consS   (car cdr) (consC (desugar car) (desugar cdr))]
    [carS    (exp)     (carC (desugar  exp)) ]
    [cdrS    (exp)     (cdrC (desugar  exp)) ]
    [displayS (exp)    (displayC (desugar exp))]
    [quoteS (sym) (quoteC sym)]
    [equal?S (val1 val2) (equal?C (desugar val1) (desugar val2))]
    [nullS  () (nullC)]
    ))

; define location (or address) as a number
(define-type-alias Location number)

; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  [nullV ]
  [quoteV (symb : symbol)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [cellV (first : (boxof Value)) (second : (boxof Value))]
  [suspV (expr : ExprC) (env : Env)]
  [boxV (address : Location)]
  )

; Defining box parameters
(define-type Storage
  [cell (location : Location) (val : (boxof Value))])
(define-type-alias Store (listof Storage))
(define override cons)
(define mt-store empty)

; Bindings associate symbol with location
(define-type Binding
        [bind (name : symbol) (loc : Location)])

; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; Find the name of a variable
(define (lookup [for : symbol] [env : Env]) : Location
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " was not found"))] ; variable is undefined
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; found it!
                                 (bind-loc (first env))]
                  [else (lookup for (rest env))])]))        ; check in the rest

; Find the value of a location on the store
(define (fetch [loc : Location] [sto : Store]) : (boxof Value)
         (cond
           [(empty? sto) (error 'fetch "Error in fetch")]
           [else (cond
                   [(equal? loc (cell-location (first sto)))
                                (cell-val (first sto))]
                   [else (fetch loc (rest sto))])]))

;Return type for the interpreter with storage
(define-type Result
        [res (val : Value) (sto : Store)])

; Auxiliary operators
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV?  l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "One of the arguments is not a number")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "One of the arguments is not a number")]))

; Returns a free location in storage
(define new-loc
  (let ([n (box 0)])
    (lambda ()
    (begin
      (set-box! n (+ (unbox n) 1))
      (unbox n)))))

; Interpreter
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    ; Numbers just evaluta to their equivalent Value
    [numC (n) (res (numV n) sto)]

    ; IDs are retrieved from the Env and unboxed
    [idC (n) (let ([return (unbox (fetch (lookup n env) sto))])
                        (if  (suspV? return)
                              (interp (suspV-expr return) (suspV-env return) sto)
                              (res return sto)
                         ))]

    ; Lambdas evaluate to closures, which save the environment
    [lamC (a b) (res (closV a b env) sto)]
    
    ; Application of function
    [appC (f a)
          (let* ([f-value (res-val (interp f env sto))]
                 [new-bind (bind (closV-arg f-value) (new-loc))])
           (interp (closV-body f-value)
              (extend-env new-bind env)
              (override (cell (bind-loc new-bind) (box (suspV a env))) sto)))]

    ; Sum two numbers using auxiliary function
    [plusC (l r) (res (let* ([left (res-val (interp l env sto))]
                             [right (res-val (interp r env sto))])
                               (if (suspV? left)
                                  (let* ([leftI (res-val (interp (suspV-expr left) env sto))])
                                     (if (suspV? right)
                                        (num+ leftI (res-val (interp (suspV-expr right) env sto)))
                                        (num+ leftI right)))
                                  (if (suspV? right)
                                      (num+ left (res-val (interp (suspV-expr right) env sto)))
                                      (num+ left right)))) sto)]

    ; Multiplies two numbers using auxiliary function
    [multC (l r) (res (let* ([left (res-val (interp l env sto))]
                             [right (res-val (interp r env sto))])
                               (if (suspV? left)
                                  (let* ([leftI (res-val (interp (suspV-expr left) env sto))])
                                     (if (suspV? right)
                                        (num* leftI (res-val (interp (suspV-expr right) env sto)))
                                        (num* leftI right)))
                                  (if (suspV? right)
                                      (num* left (res-val (interp (suspV-expr right) env sto)))
                                      (num* left right)))) sto)]

    ; Conditional operator
    [ifC (c s n) (if (zero? (numV-n (res-val (interp c env sto)))) (interp n env sto) (interp s env sto))]

    ; Sequence of operations
    [seqC (b1 b2) (begin (interp b1 env sto) (interp b2 env sto))] ; No side effect between expressions!

    ; Declaration of variable
    [letC (name arg body)
          (let* ([new-bind (bind name (new-loc))]
                 [new-env (extend-env new-bind env)]
                 [new-sto (override (cell (bind-loc new-bind) (box (suspV arg env))) sto)])
            (interp body new-env new-sto))]

    ; Use of let*
    [let*C (name1 arg1 name2 arg2 body)
           (let* ([new-bind1 (bind name1 (new-loc))]
                  [new-env1  (extend-env new-bind1 env)]
                  [new-sto1  (override (cell (bind-loc new-bind1) (box (suspV arg1 env))) sto)]
                  [new-bind2 (bind name2 (new-loc))]
                  [new-env2  (extend-env new-bind2 new-env1)]
                  [new-sto2  (override (cell (bind-loc new-bind2) (box (suspV arg2 new-env1))) new-sto1)]
                  )
             (interp body new-env2 new-sto2))]

    ; Use of letrec
    [letrecC (name arg body) (let* ([new-bind (bind name (new-loc))]
                                    [new-env (extend-env new-bind env)]
                                    [new-sto (override (cell (bind-loc new-bind) (box (suspV (numC 1) mt-env))) sto)])
                               (begin
                                 (set-box!
                                  (fetch (bind-loc new-bind) new-sto)
                                  (suspV arg new-env))
                                  (interp body new-env new-sto)))]

    ; Cell operations
    [consC (car cdr)
           (res (cellV (box (suspV car env)) (box (suspV cdr env))) sto)]
    [carC  (exp) (let* ([l (res-val (interp exp env sto))])
                       (if (suspV? (unbox (cellV-first l)))
                           (let* ([ans (interp (suspV-expr (unbox (cellV-first l))) (suspV-env (unbox (cellV-first l))) sto)])
                             (begin
                               (set-box! (cellV-first l) (res-val ans))
                               ans))
                           (res (unbox (cellV-first l)) sto)))]
    [cdrC  (exp) (let* ([l (res-val (interp exp env sto))])
                       (if (suspV? (unbox (cellV-second l)))
                           (let* ([ans (interp (suspV-expr (unbox (cellV-second l))) (suspV-env (unbox (cellV-second l))) sto)])
                             (begin
                               (set-box! (cellV-second l) (res-val ans))
                               ans))
                           (res (unbox (cellV-second l)) sto)))]
    
    ;Display values
    [displayC (exp) (let ((value (res-val (interp exp env sto))))
                      (begin (print-value (res-val (interp exp env sto)))
                             (display ";") ; no newline in plai-typed, we use ";" 
                             (res value sto)))]
    ;Symbol
    [quoteC (sym) (res (quoteV sym) sto)]
    ;Null
    [nullC  () (res (nullV) sto)]
    ;Equal?
    [equal?C (a b) (if (equal? (res-val (interp a env sto)) (res-val (interp b env sto)))
                       (res (numV 1) sto)
                       (res (numV 0) sto))]
    
    ))


; Parser
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (if (empty? sl)
           (nullS)
           (case (s-exp->symbol (first sl))
             [(+) (plusS (parse (second sl)) (parse (third sl)))]
             [(*) (multS (parse (second sl)) (parse (third sl)))]
             [(-) (bminusS (parse (second sl)) (parse (third sl)))]
             [(~) (uminusS (parse (second sl)))]
             [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
             [(call) (appS (parse (second sl)) (parse (third sl)))]
             [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
             [(seq) (seqS (parse (second sl)) (parse (third sl)))]
             [(let) (letS (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                          (parse (second (s-exp->list (first (s-exp->list (second sl))))))
                          (parse (third sl)))]
             [(let*) (let*S (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                        (parse (second (s-exp->list (first (s-exp->list (second sl))))))
                        (s-exp->symbol (first (s-exp->list (second (s-exp->list (second sl))))))
                        (parse (second (s-exp->list (second (s-exp->list (second sl))))))
                        (parse (third sl))
                        )]
             [(letrec) (letrecS (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                      (parse (second (s-exp->list (first (s-exp->list (second sl))))))
                      (parse (third sl)))]
             [(cons) (consS (parse (second sl)) (parse (third sl)))]
             [(car) (carS (parse (second sl)))]
             [(cdr) (cdrS (parse (second sl)))]
             [(display)(displayS (parse (second sl)))]
             [(quote) (quoteS (s-exp->symbol (second sl)))]
             [(equal?) (equal?S (parse (second sl)) (parse (third sl)))]
             [else (error 'parse "invalid list input")])))]
    [else (error 'parse "invalid input")]))


; Facilitator
(define (interpS [s : s-expression]) (res-val (interp (desugar (parse s)) mt-env mt-store)))

; Printing
(define (print-value [value : Value ] ) : void
                      
                      (type-case Value value
                        [numV  (n) (display n)]
                        [quoteV (symb) (display symb)]
                        [closV (arg body env)
                               (begin (display "<<")
                                      (display "lambda(")
                                      (display arg)
                                      (display ")")
                                      (display body)
                                      (display ";")
                                      (display env)
                                      (display ">>"))]
                        
                        [cellV (first second)
                               (begin (display "(")
                                      (print-list value)
                                      (display ")")
                                      )
                               ]
                        [nullV ()
                               (display '())]
                        [boxV (address) (begin (display "Box: ")
                                               (display address)
                                         )
                                     ]
                        [suspV (expr env) (display expr)]))
(define (print-list cell) : void
  (begin 
         (print-value (unbox (cellV-first cell)))
         (display " ")
         (let ([rest (unbox (cellV-second cell))])
           (type-case Value rest 
             [nullV () (display "") ]; null at the end of the list is not printed
             [cellV (first second) (print-list rest)]
             [else (begin (display ".")
                        (print-value (unbox (cellV-second cell))))]))
         )
  )

; Testes

;'(Teste letrec)
;(interpS '(letrec ([sum (lambda n (if n (+ n (call sum (- n 1))) 0))]) (call sum 5)))

;'(Teste equal?)
;(test (interpS '(equal? (lambda n (+ 1 n)) (lambda n (+ 1 n)))) (numV 1))
;'(Teste quote)
;(test (interpS '(quote alan)) (quoteV 'alan))
;'(Teste cons/car/cdr)
;'(lista '(5 4 3 2 1))
;'(car:)
;(test (interpS '(car (cons 5 (cons 4 (cons 3 (cons 2 1)))))) (numV 5))
;'(cdr:)
;(interpS '(cdr (cons 5 (cons 4 (cons 3 (cons 2 1))))))
;'(cdr com 2 elementos:)
;(test (interpS '(cdr (cons 2 1)))(numV 1))
;'(car e cdr:)
;(test (interpS '(let ([x (cons 1 (cons 2 (cons 3 4)))]) (car (cdr (cdr x))))) (numV 3))
;'(Teste call)
;(test (interpS '(let* ([func (lambda x (* x 2))] [func2 func]) (call func2 (car (cons 5 3))))) (numV 10))
;'(Teste display)
;'(Teste letrec)
;(test (interpS '(letrec ([fac (lambda n (if n (* n (call fac (- n 1))) 1))])
;                  (call fac 5)))
;      (numV 120))