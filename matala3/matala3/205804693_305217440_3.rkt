  #lang pl

;  ---------------------              TODO : comments and tests            ---------------------

; Part A

  #| BNF for the MUWAE language:
       <MUWAE> ::= <num>
               | { + <MUWAE> <MUWAE> }
               | { - <MUWAE> <MUWAE> }
               | { * <MUWAE> <MUWAE> }
               | { / <MUWAE> <MUWAE> }
               | {Sqrt <MUAWE>}
               | { with { <id> <MUWAE> } <MUWAE> }
               | <id>
  |#

  ;; MUWAE abstract syntax trees
  (define-type MUWAE
    [Num (Listof Number)]
    [Add  MUWAE MUWAE]
    [Sub  MUWAE MUWAE]
    [Mul  MUWAE MUWAE]
    [Div  MUWAE MUWAE]
    [Sqrt MUWAE]
    [Id   Symbol]
    [With Symbol MUWAE MUWAE])

  (: parse-sexpr : Sexpr -> MUWAE)
  ;; to convert s-expressions into MUWAE
  (define (parse-sexpr sexpr)
    (match sexpr
      [(list (number: n) ...) (if (null? n) (error 'parse-sexpr "bad syntax in ~s" sexpr) (Num n))]  
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'sqrt object) (Sqrt (parse-sexpr object))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

  (: parse : String -> MUWAE)
  ;; parses a string containing a MUWAE expression to a MUWAE AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  #| Formal specs for `subst':
     (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>, `y' is a
     *different* <id>)
        N[v/x]                = N
        {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
        {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
        {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
        {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
        {sqrt E}[v]              = {sqrt E[v]}
        y[v/x]                = y
        x[v/x]                = v
        {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
        {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
  |#
 
  (: subst : MUWAE Symbol MUWAE -> MUWAE)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst expr from to)
    (cases expr
      [(Num n) expr]
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(Sqrt object) (Sqrt (subst object from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body) 
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]))

  #| Formal specs for `eval':
       eval(N)         = N
       eval({+ E1 E2}) = eval(E1) + eval(E2)
       eval({- E1 E2}) = eval(E1) - eval(E2)
       eval({* E1 E2}) = eval(E1) * eval(E2)
       eval({/ E1 E2}) = eval(E1) / eval(E2)
       eval({sqrt E})   = sqrt eval(E)
       eval(id)        = error!
       eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
  |#
 ;(if (>= var 0) (list (sqrt var) (- 0 (sqrt var))) (error 'eval "`sqrt' requires a nonnegative input"))

  (: sqrt+ : (Listof Number) -> (Listof Number)) 
  (define (sqrt+ ns)  
      (cond
        [(null? ns) null]  
        [(< (first ns) 0) (error 'eval "`sqrt' requires a nonnegative input")]
        [else (cons (sqrt (first ns)) (cons (- 0 (sqrt (first ns))) (sqrt+ (rest ns))))]
     )     
   ) 

 (: bin-op : (Number Number -> Number) (Listof Number) (Listof Number) -> (Listof Number)) 
 ;; applies a binary numeric function on all combinations of numbers from 
 ;; the two input lists, and return the list of all of the results 
 (define (bin-op op ls rs) 
   (: helper : Number (Listof Number) -> (Listof Number)) 
   (define (helper l rs) 
     (: f : Number -> Number) 
    (define (f var) (op l var)) (map f rs)) 
   (if (null? ls) null (append (helper (first ls) rs) (bin-op op (rest ls) rs)))) 
 
  (: eval : MUWAE -> (Listof Number))
  ;; evaluates WAE expressions by reducing them to numbers
  (define (eval expr)
    (cases expr 
      [(Num n) n]   
      [(Add l r) (bin-op + (eval l) (eval r))]
      [(Sub l r) (bin-op - (eval l) (eval r))]
      [(Mul l r) (bin-op * (eval l) (eval r))] 
      [(Div l r) (bin-op / (eval l) (eval r))] 
      [(Sqrt e) (sqrt+ (eval e))] 
      [(With bound-id named-expr bound-body)
       (cons (first (eval (subst bound-body
                    bound-id  
                    (Num (eval named-expr))))) '())]
      [(Id name) (error 'eval "free identifier: ~s" name)]))

  (: run : String -> (Listof Number))
  ;; evaluate a MUWAE program contained in a string
  (define (run str)
    (eval (parse str)))
   
  ;; tests
  (test (run "{0 0 0 0 0 0 0 0 0 0}") => '(0 0 0 0 0 0 0 0 0 0))
  (test (run "{}") =error> "bad syntax in ()")
  (test (run "{{}{}{}{}{}{}}") =error> "bad syntax in (() () () () () ())")
  (test (run "x")=error> "free identifier")
  (test (run "{+ {0} {0}}") => '(0))
  (test (run "{- {3 4} {1 2}}") => '(2 1 3 2))
  (test (run "{* {1 2} {3 4}}") => '(3 4 6 8))
  (test (run "{/ {2 3} {1 1}}") => '(2 2 3 3))
  (test (run "{5}") => '(5))
  (test (run "{+ {5} {5}}") => '(10))
  (test (run "{with {x {+ {5} {5}}} {+ x x}}") => '(20))
  (test (run "{with {x {5}} {+ x x}}") => '(10))
  (test (run "{with {x {+ {5} {5}}} {with {y {- x {3}}} {+ y y}}}") => '(14))
  (test (run "{with {x {5}} {with {y {- x {3}}} {+ y y}}}") => '(4))
  (test (run "{with {x {5}} {+ x {with {x {3}} {10}}}}") => '(15))
  (test (run "{with {x {5}} {+ x {with {x {3}} x}}}") => '(8))
  (test (run "{with {x {5}} {+ x {with {y {3}} x}}}") => '(10))
  (test (run "{with {x {5}} {with {y x} y}}") => '(5))
  (test (run "{with {x {5}} {with {x x} x}}") => '(5))
  (test (run "{with}") =error> "bad `with' syntax in (with)")
  (test (run "{with {x}}") =error> "bad `with' syntax in (with (x))")
  (test (run "{with {x 5} {}}") =error> "bad syntax in 5")
  (test (run "{with 5}") =error> "bad `with' syntax in (with 5)")
  (test (run "{with x y z}") =error> "bad `with' syntax in (with x y z)")
  (test (run "{x y z}") =error>  "bad syntax in (x y z)")
  (test (run "{678}") => '(678))
  (test (run "{-678}") => '(-678))
  (test (run "{with {x {1}} y}") =error> "free identifier")
  (test (run "{sqrt {9}}") => '(3 -3))
  (test (run "{sqrt {1}}") => '(1 -1))
  (test (run "{sqrt {0}}") => '(0 0))  
  (test (run "{sqrt {-1}}") =error> "`sqrt' requires a nonnegative input")
  (test (run "{sqrt {-0}}") => '(0 0)) 
  (test (run "{+ {sqrt {1}} {3}}") => '(4 2))
  (test (run "{- {sqrt {9}} {sqrt {4}}}") => '(1 5 -5 -1))
  (test (run "{* {sqrt {9}} {sqrt {4}}}") => '(6 -6 -6 6))
  (test (run "{/ {sqrt {9}} {sqrt {1}}}") => '(3 -3 -3 3))
  (test (run "{+ x y}") =error>  "free identifier")
  (test (run "{+ {/ {+ {sqrt {1}} {3}} {2}} {sqrt {100}}}") => '(12 -8 11 -9))
  (test (run "{+ {+ {sqrt {1}} {sqrt {1}}} {+ {sqrt {1}} {sqrt {1}}}}") => '(4 2 2 0 2 0 0 -2 2 0 0 -2 0 -2 -2 -4))   
  (test (run "{sqrt {+ {16} {* {+ {1} {sqrt {1}}} {/ {9} {2}}}}}") => '(5 -5 4 -4))
  (test (run "{sqrt {16}}") => '(4 -4))
  (test (run "{/ {sqrt {16}} {2}}") => '(2 -2))
  (test (run "{* {sqrt {16}} {2}}") => '(8 -8)) 
  (test (run "{/ 0 0}") =error>  "bad syntax in 0")

; Part B

#|

<WAE> ::= <num> 
   | {+ <WAE> <WAE>}
   | {-  <WAE> <WAE>}
   | {* <WAE> <WAE>}
   | {/ <WAE> <WAE>}
   | {with {<id> <WAE>} <WAE>}
   | <id>

|#

(define-type WAE
  [NumW Number]
  [AddW WAE WAE]
  [SubW WAE WAE]
  [MulW WAE WAE]
  [DivW WAE WAE]
  [IdW Symbol]
  [WithW Symbol WAE WAE])

(: parse-sexprW : Sexpr -> WAE) 
(define (parse-sexprW sexpr)
  (match sexpr
    [(number: n) (NumW n)]
    [(symbol: name) (IdW name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithW name (parse-sexprW named) (parse-sexprW body))]
       [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (AddW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '- lhs rhs) (SubW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '* lhs rhs) (MulW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '/ lhs rhs) (DivW (parse-sexprW lhs) (parse-sexprW rhs))]
    [else (error 'parse-sexprW "bad syntax in ~s" sexpr)]))
 
(: parseW : String -> WAE)
(define (parseW str)
  (parse-sexprW (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

(: substW : WAE Symbol WAE -> WAE)
(define (substW expr from to)
  (cases expr
    [(NumW n) expr]
    [(AddW l r) (AddW (substW l from to) (substW r from to))]
    [(SubW l r) (SubW (substW l from to) (substW r from to))]
    [(MulW l r) (MulW (substW l from to) (substW r from to))]
    [(DivW l r) (DivW (substW l from to) (substW r from to))]
    [(IdW name) (if (eq? name from) to expr)]
    [(WithW bound-id named-expr bound-body)
     (WithW bound-id
           (substW named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (substW bound-body from to)))]
    ))

(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList expr)
  (cases expr
    [(NumW number:n) '()] 
    [(AddW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(SubW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(MulW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(DivW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(IdW name) (list name)]
    [(WithW bound-id named-expr bound-body)
     (cases named-expr
       [(NumW n)
        (if (null? (member bound-id (freeInstanceList bound-body)))
           (cons bound-id (freeInstanceList bound-body)) 
           (remv bound-id (freeInstanceList bound-body)))]
       [else (append (freeInstanceList (IdW bound-id)) (freeInstanceList named-expr) (freeInstanceList bound-body))]
      ) 
    ]
  ) 
)

(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (AddW (IdW 'x) (NumW 3))) => '(x))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
(test (freeInstanceList (parseW "{with {x {+ p l}} {+ y z}}")) => '(x p l y z))
(test (freeInstanceList (parseW "5")) => '())
(test (freeInstanceList (NumW 5)) => '())
(test (freeInstanceList (AddW (NumW 2) (IdW 'x))) => '(x))
(test (freeInstanceList (parseW "{+ 2 x}")) => '(x))
(test (freeInstanceList (parseW "{+ 2 {with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}}")) => '(xx y z))
(test (freeInstanceList (parseW "{+ y {with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}}")) => '(y xx y z))
(test (freeInstanceList (AddW (IdW 'xxx) (IdW 'yyy))) => '(xxx yyy))
(test (freeInstanceList (AddW (NumW 3) (IdW 'vvv))) => '(vvv))
(test (freeInstanceList (AddW (IdW 'xxx) (IdW 'yyy))) => '(xxx yyy))
(test (freeInstanceList (SubW (IdW 'xxx) (IdW 'yyy))) => '(xxx yyy))
(test (freeInstanceList (MulW (IdW 'xxx) (IdW 'yyy))) => '(xxx yyy))
(test (freeInstanceList (DivW (IdW 'xxx) (IdW 'yyy))) => '(xxx yyy))
(test (freeInstanceList (IdW 'xx)) => '(xx))
(test (freeInstanceList (WithW 'x (IdW 'y) (IdW 'z))) => '(x y z))