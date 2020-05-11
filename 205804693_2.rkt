#lang pl

;;;;;;         TODO :  2.2 , comments

; Question 1

; 1.1
;; In this question i wrote some BNF rules to ROL language , the ROL objects defined too to make the BNF learning easier

#|
                                                                                         BNF for the RegE language

      <ROL>  ::= {reg-len = <num> <RegE>}                         
      <RegE> ::= <Bits> | {or <RegE> <RegE>>} | {and <RegE> <RegE>} | {shl <RegE>}
      <Bits> ::= 1 <Bits> | 0 <Bits> | 0 | 1

                                                                                           3 examples for ROL program codes

      A.    ROL : { reg-len =  4  {1 0 0 0}} =>> ROL -> {reg-len = <num> <RegE>} -> {reg-len = 4 <Bits>} -> {reg-len = 4 {1 <Bits>}} -> {reg-len = 4 {1 0 <Bits>}} ->
                       {reg-len = 4 {1 0 0 <Bits>}} -> {reg-len = 4 {1 0 0 0}}

      B.    ROL : { reg-len = 4  {shl {1 0 0 0}}} =>> ROL -> {reg-len <num> <RegE>} -> {reg-len = 4 {shl <RegE>}} -> {reg-len = 4 {shl <Bits>}} -> {reg-len = 4 {shl 1 <Bits>}} ->
                       {reg-len = 4 {shl 1 0 <Bits>}} -> {reg-len = 4 {shl 1 0 0 <Bits>}} -> {reg-len = 4 {shl 1 0 0 0}}

     C.     ROL : { reg-len = 4  {and {shl {1 0 1 0}}{shl {1 0 1 0}}}} =>> ROL -> {reg-len <num> <RegE>} -> {reg-len 4 <RegE>} -> {reg-len 4 {and <RegE> <RegE>}} ->
                       {reg-len 4 {and {shl RegE} {shl RegE}}} -> {reg-len 4 {and {shl <Bits>} {shl <Bits>}}} -> {reg-len 4 {and {shl 1 <Bits>} {shl 1 <Bits>}}} ->
                       {reg-len 4 {and {shl 1 0 <Bits>} {shl 1 0 <Bits>}}} -> {reg-len 4 {and {shl 1 0 1 <Bits>} {shl 1 0 1 <Bits>}}} -> {reg-len 4 {and {shl 1 0 1 0} {shl 1 0 1 0}}}

|#

; 1.2

(define-type BIT = (U 0 1))

(define-type Bit-List = (Listof BIT))

; In this exercise, I wanted to simplify the RegE type,
; so I defined the constructors as the simplest cases that are considered valid expressions in the language.
; The 'bit list' case is completely the base of the recoursive type 'RegE'.

(define-type RegE 
    [Reg Bit-List] 
    [And RegE RegE] 
    [Or RegE RegE] 
    [Shl RegE]
)

(: list->bit-list : (Listof Any) -> Bit-List)

(define (list->bit-list lst)
  (cond
    [(null? lst) null]
    [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
    [else (cons 0 (list->bit-list (rest lst)))]
   )
)
; This is the function that actually checks valid writing in language and only then transfers to a computation that calculates the desired expression,
; which is basically some sort of classic debug function for cases that dictate writing rules for language.
; We note that here we refer to 'reg-len and '= as a symbol only.
; That is, they have no effect in calculating the expression but only in the legality of the written sentence.
(: parse-sexpr : Sexpr -> RegE)

(define (parse-sexpr sexpr)
  (match sexpr
    [(list 'reg-len '= (number: n) lst) (parse-sexpr-RegL lst n)]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]
  )
)

; Here, basically, the recursive calculation of the RegE expression is done using a base case that indicates a bit list and a real number.
; All this is done only and when the number we referenced is completely equal to the bit list which is also part of the input.
; In addition, one of the most important actions is to convert the expression to the desired one.
; Here, too, the realization is completely recursive and adapts to the nature of the language and of course to the type we designed.

(: parse-sexpr-RegL : Sexpr Number -> RegE)

(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr    
  [(list (and a (or 1 0)) ... ) (if (= (length a) reg-len) (Reg (list->bit-list a)) (error 'parse-sexpr "wrong number of bits in ~s" a))] 
  [(list 'and left right) (And (parse-sexpr-RegL left reg-len) (parse-sexpr-RegL right reg-len))]
  [(list 'or left right) (Or (parse-sexpr-RegL left reg-len) (parse-sexpr-RegL right reg-len))]
  [(list 'shl object) (Shl (parse-sexpr-RegL object reg-len))]
  [else (error 'parse-sexpr-RegL "bad syntax in ~s" sexpr)])) 
 
(: parse : String -> RegE)

 (define (parse str)
 (parse-sexpr (string->sexpr str))
) 
  
(test (parse "{ reg-len =  4  {1 0 0 0}}") => (Reg '(1 0 0 0)))
(test (parse "{ reg-len = 4  {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
(test (parse "{ reg-len = 4  {and {1 0 0 0} {0 1 1 1}}}") => (And (Reg '(1 0 0 0)) (Reg '(0 1 1 1)))) 
(test (parse "{ reg-len = 4  {or {1 0 0 0} {0 1 1 1}}}") => (Or (Reg '(1 0 0 0)) (Reg '(0 1 1 1))))
(test (parse "{ reg-len = 4  {and {or {1 0 0 0} {0 1 1 1}} {or {0 1 1 1} {1 1 1 0}}}}") => (And (Or (Reg '(1 0 0 0)) (Reg '(0 1 1 1))) (Or (Reg '(0 1 1 1)) (Reg '(1 1 1 0)))))
(test (parse "{ reg-len = 4  {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
(test (parse "{ reg-len = 4  { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
(test (parse "{ reg-len = 2  { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
(test (parse "{ reg-len = 3  { or {and {shl {1 0 0}} {1 0 0}} {1 0 0}}}") => (Or (And (Shl (Reg '(1 0 0))) (Reg '(1 0 0))) (Reg '(1 0 0))))
(test (parse "{ reg-len = 1 {1}}") => (Reg '(1)))
(test (parse "{ reg-len = 2 {1 0}}") => (Reg '(1 0))) 
(test (parse "{ reg-len = 2 {+ {2 3} {4 5}}}")=error> "bad syntax in")
(test (parse "{ reg-len = 2 {+ {1 0} {0 1}}}")=error> "bad syntax in")
(test (parse "{ reg-len = 2 {- {1 0} {0 1}}}")=error> "bad syntax in") 
(test (parse "{ reg-len = 2 {/ {1 0} {0 1}}}")=error> "bad syntax in")
(test (parse "{ reg-len = }")=error> "bad syntax in")
(test (parse "{ {/ {1 0} {0 1}}}")=error> "bad syntax in")
(test (parse "{ reg-len }")=error> "bad syntax in")
(test (parse "{ {1 0 0} 3 = reg-len }")=error> "bad syntax in") 
(test (parse "{ = }")=error> "bad syntax in")
(test (parse "{ 5 }")=error> "bad syntax in")
(test (parse "{ reg-len = 2 {* {1 0} {0 1}}}")=error> "bad syntax in")
(test (parse "{ reg-len = 1 {or {1 0} {0}}}")=error> "wrong number of bits in") 
(test (parse "{ reg-len = 0  {1}}")=error> "wrong number of bits in") 
(test (parse "{ reg-len = 4  {or {1 1 1 1} {0 1 1}}}")=error> "wrong number of bits in") 
 

; Question 2

; 2.1

#|

First of all i parsed the expression ' {* {+ {set 1} {set 2}} get} ' according to derivation rules to
                                                                                                             {* {+ {set <num>} {set <num>}} get}
                                                                                                         -> {* {+ {set <num>} {set <num>}} get}
                                                                                                         -> {* {+ {set <MAE>} {set <MAE>}} get}
                                                                                                         -> {* {+ <MAE> <MAE>} <MAE>}
                                                                                                         -> {* <MAE> <MAE>}
                                                                                                         -> <MAE>.
By parsing the above expression I can understand that there is a problem defining the set functionality
as it is not entirely clear and therefore using it can lead to unwanted value.
That is, 'get' is not properly configured because it should receive an address and return the stored value ,
of course it does not fulfill the functionallity goal described at the paper (get some value from cell memory).
In addition, 'set' gets free numbers here even though I should get an E expression
that should evaluate and finally store (I'm not so sure that's a problem, that's what I understand from the role definition).

; 2.2.1
;;;;;  'set valid around expressions only' according to the paper ;;;;;

New BNF grammar to MAE :

<GetReal> ::= <num> | get

<Get-Set> :: = <num> |  {+ {<GetReal>} {<GetReal>}} | {- {<GetReal>} {<GetReal>}} | {* {<GetReal>} {<GetReal>}} | {/ {<GetReal>} {<GetReal>}}

<Set> ::= {set {<Get-Set>}}                      

<SetGetRecoursive> ::= {<set> <SetGetRecoursive>} | {get <SetGetRecoursive>} | {}

<Expressions> ::= {<set> <SetGetRecoursive>} | {get <SetGetRecoursive>}

<MAE> ::= {seq <Expressions>}

|#

; 2.2.2

; Have to implement example to derivative process here

; Question 3
; Square implementation using foldl functionallity

; At this functionallity implementation we were asked to use the known function 'foldl'.
; Help to the map and the square function I wrote above the desired function
; I was able to run foldl on the list after doing the list arguments transformation to (square value) of each argument value on the list.
; The function receives a list of numbers and returns a number accordingly.

 (: square : Number -> Number)
(define (square x) (* x x))

(: sum-of-squares : (Listof Number) -> Number)

(define (sum-of-squares list)
  (cond
    [(= (length list) 0) (error 'sum-of-squares "failed because ~a" "the list is empty")]
    [else (foldl + 0 (map square list))]
  ) 
) 

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 2 3)) => (sum-of-squares '(-1 -2 -3)))
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '())=error> "failed because the list is empty")

; Question 4

; 4.a Define the BINTREE type
; Here, I have defined a new binary tree type that has three modes (or in other words constructors).
; I tried to simplify as much as possible so I gave the most simple situations :
; Empty tree
; One leaf tree
; Tree with 2 nodes
; Every consturctor present kind of binary tree body and according to that and the racket recoursive style i refer to each tree as a node.

(define-type BINTREE
  [Node BINTREE BINTREE]
  [Leaf Number]
  [EmptyTree]
)
 
; 4.b Tree map with Unary function
; This function takes on a unary function and a tree, its purpose is to edit the values ​​of the tree organs by activating
; the unary function on every value and value in the tree.
; This function is especially useful, sometimes even more so than the forEach function that
; returns no value ... Here the function makes the changes and returns a new organ collection.

(: tree-map : (Number -> Number) BINTREE -> BINTREE) 

(define (tree-map f Tree)
  (cases Tree 
    [(Leaf value) (Leaf (f value))]  
    [(Node left right) (Node (tree-map f left) (tree-map f right))]
    [(EmptyTree) (error 'tree-reverse "failed because ~a" "the tree is empty")]
    )   
 )       
 
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map (lambda (x) (- x 1)) (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))) => (Node (Leaf 0) (Node (Leaf 1) (Leaf 2)))) 
(test (tree-map add1 (EmptyTree))=error> "failed because the tree is empty")

; 4.d Tree foldl implementation
; In this function reads something much more interesting than its predecessors,
; it involves some complexity on the tree arguments and, one function that is supposed to act on the argument values ​​that change according
; to the second position that changes their values.
; Here, the function receives two functions and a tree and is supposed to run one of the functions on the tree arguments and
; one is supposed to run on the results of the first function run on the arguments.

(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))

(define (tree-fold f g tree)
  (cases tree 
    [(Leaf value) (g value)]   
    [(Node left right) (f (tree-fold f g left) (tree-fold f g right))]
    [(EmptyTree) (error 'tree-reverse "failed because ~a" "the tree is empty")]
  )   
)

(: func-test : Number -> Number)
(define (func-test x) (* x 2))

(test (tree-fold + func-test (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => 12)
(test (tree-fold * func-test (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => 48)
(test (tree-fold + func-test (EmptyTree))=error> "failed because the tree is empty")

; 4.e  Tree flatten to list
; I have no explanations,
; a function I took from the assignment explanation.

(: tree-flatten : BINTREE -> (Listof Number))

(define (tree-flatten tree)
 (tree-fold (inst append Number) (inst list Number) tree)
 )

(test (tree-flatten (Node (Leaf 2) (Node (Leaf 3) (Leaf 4)))) => '(2 3 4))
(test (tree-flatten (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)))) => (sort (tree-flatten (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 4) (Leaf 1)))) <))
(test (tree-flatten (EmptyTree))=error> "failed because the tree is empty")
 
; 4.g  Tree reverse

; In this function I used a very simple realization, changed the sides of the Nodes into a tree and thus created a mirror for the tree.
; the leaf shows his mirror so I returned the same leaf exactly.
; This function gets as a tree input and returns a tree type object that is a mirror to the tree received as an input.
; Using recursive flatting and converting the tree to the list we can use down tests,
; this is a special useful functions (flatt list) that integrates with the transformation of the tree.

(: switch-nodes : BINTREE BINTREE -> BINTREE)

(define (switch-nodes a b)
  [Node b a] 
)

(: tree-reverse : BINTREE -> BINTREE)

(define (tree-reverse tree) 
  (tree-fold switch-nodes Leaf tree)
) 
 
(test (reverse (tree-flatten (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))) => 
        (tree-flatten (tree-reverse (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))))
(test (tree-reverse (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3))) => (Node (Leaf 3) (Node (Leaf 2) (Leaf 1))))
(test (tree-reverse (EmptyTree))=error> "failed because the tree is empty")
