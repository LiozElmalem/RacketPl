#lang pl

 (require  racket/list)
 (require  racket/string)

 ;; Question 1

 (: plSuffixContained : (Listof String) -> Any)

 ; Input - list of strings , Output - #f or the current suffix that founded.
 ; Recoursive function that implements by simple stop condition and first and last options that give us the recoursive iterative.
 ; In addition , the 'string-suffix' functionality help me to solve it easily.
 (define (plSuffixContained list)
       (if (empty? list)
          false  
          (if (string-suffix? (first list) "pl") (first list) (plSuffixContained (rest list)))
       )  
 )       
  
 (test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT"
    "lol")) => false)

 ;; Question 2.1

 (: write-poly : (Listof Number) -> String)

 ; Input - list of numbers , Output - polynomial string.
 ; Like the previous function, I use stop conditions here, but some are unlike the previous one.
 ;The let operation declare the initialize output , every stop condition here decide the string output body.
 ; Functions like 'string-append' very useful here to decrease the code lines.
 (define (write-poly list)
  (let ([output ""])
    (cond
      [(empty? list) output]
      [(= 2 (length list)) (string-append output (number->string (first list)) "x+" (write-poly (rest list)))] 
      [(= 1 (length list)) (string-append output (number->string (first list)) (write-poly (rest list)))]
      [else (string-append output (number->string (first list))  "x^" (number->string (- (length list) 1)) "+" (write-poly (rest list)))]
 ))) 
 
 (test (write-poly '(3 2 6)) => "3x^2+2x+6")
 (test (write-poly '()) => "")
 (test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")

 ;; Question 2.2

 (: compute-poly : Number (Listof Number) -> Number)

 ; Input - list of number and other number , Output - series of calculation that given (x^n * list(n) + ..... + x^0 * list(0)).
 ; Same implementation to the previous function.
 ; Tail recoursive style shown here in the body of the condition statments.
 ; NEED TO IMPLEMENT IT IN TAIL RECOURSIVE
 (define (compute-poly num list)
      (cond
         [(empty? list) 0]
         [else (+ (* (first list) (expt num (- (length list) 1))) (compute-poly num (rest list)))]
 ))    
   
 (test (compute-poly 2 '()) => 0) 
 (test (compute-poly 2 '(3 2 6)) => 22)
 (test (compute-poly 3 '(4 3 -2 0)) => 129)
 
 ;; Question 3

 ; Classic define of type 'KeyStack' with 2 constructors.
 ; The constructors show us 2 body styles of 'KeyStack'.
 (define-type KeyStack 
   [EmptyKS]
   [Push Symbol String KeyStack]
  )

 (: search-stack : Symbol KeyStack -> Any)

 ; Input - symbol and stack , Output - #f or string that we looking for.
 ; Using cases here for type 'KeyStack'.
 ; The equalitaion between the sym and symbol variabales decide which answer need to return.
 (define (search-stack symbol stack)
   (cases stack 
        [(EmptyKS) #f]
        [(Push sym x stack) (if (eq? symbol sym) x #f)]
     )
  )

 (: pop-stack : KeyStack -> Any)

 ; Input - stack , Output - #f if empty stack or the current stack without the last item that we added.
 ; Every pop functionality based on this condition statments , the empty case and the negative one.
 ; Simple implementation of pop item function , return the stack pointer.
 (define (pop-stack stack)
   (cases stack 
        [(EmptyKS) #f]
        [(Push sym x stack) stack]
     )
 ) 

 ; Stack tests
(test (EmptyKS) => (EmptyKS))
  
 (test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>  
      (Push 'b "B" (Push 'a "A" (EmptyKS)))) 
 (test (Push 'a "AAA" (Push 'b "B" (Push 'a "A"  (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) 

 (test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA") 
 (test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f) 

 (test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS)))) 
 (test (pop-stack (EmptyKS)) => #f) 
 
 ;; Question 4

 (: is-odd? : Natural -> Boolean)
 ;; << Add your comments here>> 
 ;; << Add your comments here>>
 (define (is-odd? x)
   (if (zero? x) 
      false
      (is-even? (- x 1))
      )
   ) 
 
 (: is-even? : Natural -> Boolean)
 ;; << Add your comments here>> 
 ;; << Add your comments here>>
 (define (is-even? x)
   (if (zero? x)
      true
      (is-odd? (- x 1))
      )
   ) 
 
 ;; tests --- is-odd?/is-even?
 (test (not (is-odd? 12)))
 (test (is-even? 12)) 
 (test (not (is-odd? 0)))
 (test (is-even? 0))
 (test (is-odd? 1))
 (test (not (is-even? 1))) 
 
 (: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
 ;; See explanation about the All syntax at the end of the file… 
 ;; << Add your comments here>> ;; << Add your comments here>>
 (define (every? pred lst)
    (or (null? lst)
       (and (pred (first lst))
           (every? pred (rest lst))
           )
       )
   ) 
 
 
 ;; An example for the usefulness of this polymorphic function
 (: all-even? :   (Listof Natural) -> Boolean)
 ;; << Add your comments here>>
 ;; << Add your comments here>>
 (define (all-even? lst)
   (every? is-even? lst)) 
 
 
;; tests (test (all-even? null))
 (test (all-even? (list 0)))
 (test (all-even? (list 2 4 6 8)))
 (test (not (all-even? (list 1 3 5 7))))
 (test (not (all-even? (list 1))))
 (test (not (all-even? (list 2 4 1 6)))) 
 
  
 (: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
 ;; << Add your comments here>>
 ;; << Add your comments here>>
 (define (every2? pred1 pred2  lst1 lst2)
   (or (null? lst1)
      ;; both lists assumed to be of same length
      (and (pred1 (first lst1))
          (pred2 (first lst2))
          (every2? pred1 pred2 (rest lst1) (rest lst2))
          )
      )
   ) 
 