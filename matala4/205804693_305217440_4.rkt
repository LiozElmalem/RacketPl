;; The Flang interpreter

#lang pl 

#|
Authors : Gal Caspi and Lioz Elmalem

;; We extend our BNF and Parser to Support this syntax. we notice that `then-do’ and `else-do’ are terminals and are part of the syntax of the “if” special form.
;; We used the previous assignments to achieve the goal and completion of the BNF Rules
;; We notice that to fill in the if statment we need to add a if <FLANG>  Conditions, and Conditions are { then-do <FLANG> }  { else-do <FLANG> }.
;; This part of the question did not took us more than 30 min - after we finish the assignments we return to change a bit the BNF Rules to get the best grammar for out task.
#|
The grammar:
<FLANG> ::= <num> ;; Rule 1
| { + <FLANG> <FLANG> } ;; Rule 2
| { - <FLANG> <FLANG> } ;; Rule 3
| { * <FLANG> <FLANG> } ;; Rule 4
| { / <FLANG> <FLANG> } ;; Rule 5
| { with { <id> <FLANG> } <FLANG> } ;; Rule 6
| <id> ;; Rule 7
| { fun { <id> } <FLANG> } ;; Rule 8
| { call <FLANG> <FLANG> } ;; Rule 9
| { <True> } ;; True ;; Rule 10
| { <False> }
;; Rule 11
| { = <FLANG> <FLANG> } ;; add rule for =
;; Rule 12
| { not <FLANG> }
;; Rule 13
| { < <FLANG> <FLANG> }
;; Rule 14
| { > <FLANG> <FLANG> }
;; Rule 15
| { if <FLANG> Conditions } ;; add rule 16 for (the above) if
;; Conditions  ;; Rule 16 
| { { then-do <FLANG> }  { else-do <FLANG> } } ;; 

|#

  Evaluation rules:

    subst:
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {call E1 E2}[v/x]     = {call E1[v/x] E2[v/x]}
      {fun {y} E}[v/x]      = {fun {y} E[v/x]}           ; if y =/= x
      {fun {x} E}[v/x]      = {fun {x} E}

    eval:
      eval(N)            = N
      eval({+ E1 E2})    = eval(E1) + eval(E2)  \ if both E1 and E2
      eval({- E1 E2})    = eval(E1) - eval(E2)   \ evaluate to numbers
      eval({* E1 E2})    = eval(E1) * eval(E2)   / otherwise error!
      eval({/ E1 E2})    = eval(E1) / eval(E2)  /
      eval(id)           = error!
      eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
      eval(FUN)          = FUN ; assuming FUN is a function expression
      eval({call E1 E2}) = eval(Ef[eval(E2)/x]) if eval(E1)={fun {x}Ef}
                         = error!               otherwise
  |#

;; We use the tests examples to complete the missing parts of the FLANG type definition and the parse-sexpr procedure.
;; We got the arith-op of FLANG and than we strated with the logic-op
;; Also we noticed that If constrator needs to have 3 FLANG (condition , expression , expression)
;; To accomplish this part it took us 10 min.

(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [Id   Symbol]
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  [Call FLANG FLANG]
  [Bool Boolean]
  [Bigger FLANG FLANG]
  [Smaller FLANG FLANG]
  [Equal FLANG FLANG]
  [Not FLANG]
  [If FLANG FLANG FLANG])

;;In the recursive way in which Racket works we based our work on working with bool exp and in addition to num exp.
;;we noticed the variables and difficulties along the way, the time took us more than 1 hour.
;;we wanted to model the bool variables like how FLANG works with Num and so achieving the goal was much more convenient.
;;and so we implemented it to convert s-expressions into FLANGs.
(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    ['True (Bool true)] ;; Cheking condstion if 'True -> true  
    ['False (Bool false)]  ;; also with 'Flase -> false
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'not exp) (Not (parse-sexpr exp))]
    [(cons 'if more)
     (match sexpr
       [(list 'if condition (list 'then-do l) (list 'else-do r))
        (If (parse-sexpr condition) (parse-sexpr l) (parse-sexpr r))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: subst : FLANG Symbol FLANG -> FLANG)
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
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
    [(Call l r) (Call (subst l from to) (subst r from to))]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
    [(Bool b) expr]
    [(Equal l r) (Equal (subst l from to) (subst r from to))]
    [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
    [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
    [(Not expr) (Not (subst expr from to))] 
    [(If condition then-do else-do) (If (subst condition from to) (subst then-do from to) (subst else-do from to))]
    ))

;; The same implementation way of Num->number
(: flang->bool : FLANG -> Boolean)
(define (flang->bool e)
  (cases e
    [(Bool b) b] 
    [else (error 'flang->bool "expected a boolean, got: ~s" e)])) 

(: Num->number : FLANG -> Number)
;; gets a FLANG -- presumably a Num variant -- and returns the
;; unwrapped number
(define (Num->number e)
  (cases e
    [(Num n) n]
    [else (error 'Num->number "expected a number, got: ~s" e)]))

(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
  (Num (op (Num->number expr1) (Num->number expr2))))


;; Here we implement the logic-op function
;; Function : logic-op
;; Comsumes : (Number Number -> Boolean) FLANG FLANG
;; Return : FLANG
;; Method : We accomplish that with the help of the arith-op fun , Cheking the Bool cond between to nums
(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
;; gets a Racket Boolean binary operator (on numbers), and applies it
;; to two `Num' wrapped FLANGs
(define (logic-op op expr1 expr2)
  (Bool (op (Num->number expr1) (Num->number expr2))))

;; As Racket works recursively on the arith-op we wanted to implement it recursively on the logic-op as well.
;; We implemented the Type FLANG Constrators and thus modeled the FLANG expr.
;; By modeling the bool exprs, we have been able to easily and effectively implement the evel on the logic-op expressions
;; the time it took us to finish the evel func was 1 hour
(: eval : FLANG -> FLANG)
;; evaluates FLANG expressions by reducing them to *expressions*
(define (eval expr)  
  (cases expr       
    [(Num n) expr]  
    [(Add l r) (arith-op + (eval l) (eval r))]
    [(Sub l r) (arith-op - (eval l) (eval r))]
    [(Mul l r) (arith-op * (eval l) (eval r))]
    [(Div l r) (arith-op / (eval l) (eval r))]  
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (eval named-expr)))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Fun bound-id bound-body) expr] 
    [(Call fun-expr arg-expr)
     (let([fval (eval fun-expr)])
       (cases fval   
         [(Fun bound-id bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval arg-expr)))]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]
    [(Bool b) expr]
    [(Equal l r) (logic-op = (eval l) (eval r))]
    [(Bigger l r) (logic-op > (eval l) (eval r))]
    [(Smaller l r) (logic-op < (eval l) (eval r))]
    [(Not e) (Bool (not (flang->bool (eval e))))]
    [(If l m r)
     (let ([val (eval l)])
       (cases val
         [(Bool b) (if b (eval m) (eval r))]
         [(Num n) (if (zero? n) (eval m) (eval r))]
         [else (error 'eval "bad if syntax ~s" val)]
         ))]
    ))  

;; Function:  run
;; Consumes :   String
;; Return :   U Number Boolean FLANG
;; Method :  this function run evaluate a FLANG program contained in a string
;; we added (by the tests) to this function the cases of : Bool b -> will return b (a boolean)  , Fun a b -> will run the evaluation of 2 args.
;; the time it took us to accomplish the run function was not more than 10 min
(: run : String -> (U Number Boolean FLANG))
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str))])
    (cases result
      [(Num n) n]
      [(Bool b) b]
      [(Fun a b) (Fun a b)]
      [else (error 'run "evaluation returned a non-number or boolean or FLANG: ~s" result)])))


;; tests
(test (run "{call {fun {x} {+ x 1}} 4}") => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}} {call add3 1}}") => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}} {with {add1 {fun {x} {+ x 1}}} {with {x 3} {call add1 {call add3 x}}}}}") => 7)
(test (run "True") => true)
(test (run "False") => false)
(test (run "{not True}") => false)
(test (run "{not False}") => true)
(test (run "{not {not {not False}}}") => true)
(test (run "{> {* 5 6} {/ 100 3}}") => false)
(test (run "{< {* 5 6} {/ 100 3}}") => true)
(test (run "5") => 5)
(test (run "{+ 4 4}") => 8)
(test (run "{- 4 4}") => 0)
(test (run "{/ 4 4}") => 1)
(test (run "{* 4 4}") => 16)
(test (run "{> 3 44}") => false)
(test (run "{if {+ 3 4} {then-do 5} {else-do 4}}") => 4)
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{with {x 8} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 100} {if {= x 100} {then-do 1} {else-do x}}}") => 1)
(test (run "{with {x 0} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{with {c True} {if c {then-do {> 2 1}} {else-do 2}}}") => true)
(test (run "{with {foo {fun {x} {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")=>(Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{with {x 0} {if {> x 0} {/ 2 x} x}}")=error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
(test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}")=error> "eval: free identifier: false")
(test (run "lioz_and_gal")=error> "eval: free identifier: lioz_and_gal")
(test (run "{< False 5}")=error> "Num->number: expected a number, got: #(struct:Bool #f)")
(test (run "{not 5}")=error> "flang->bool: expected a boolean, got: #(struct:Num 5)")
(test (run "{with x {not 6}}")=error> "bad `with' syntax in (with x (not 6))")
(test (run "{fun 7 {if {< x 2} {then-do x} {else-do {/ x 2}}}}")=error> "bad `fun' syntax in" )
(test (run "{>= 9 8}")=error> "bad syntax in")
(test (run "{<= 9 8}")=error> "bad syntax in")
(test (run "{-= 9 8}")=error> "bad syntax in" )
(test (run "{if {> 1 1} {then-do False} {else-do {+ 2 2}}}") => 4)
(test (run "{with {a False} {if a {then-do {> 2 1}} {else-do 2}}}") => 2)
(test (run "{+= 9 8}")=error> "bad syntax in" )
(test (run "{*= 9 8}")=error> "bad syntax in" )
(test (run "{/= 9 8}")=error> "bad syntax in" )
(test (run "{with {foo {fun {x} {if {> x 2} {then-do x} {else-do {/ x 2}}}}} foo}")=>(Fun 'x (If (Bigger (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{call {fun {x} {if {> x 3} {then-do {* x x}} {else-do 0}}} 4}") => 16)
(test (run "{+ {if {> 0 9} {then-do 1} {else-do 2}} 2}") => 4)
(test (run "{if {not True} {then-do False} {else-do True}}") => true)
(test (run "{not {> 0 0}}") => true)