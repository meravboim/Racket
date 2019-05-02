#lang pl 02 

#|
question 1 - BNF (SE)
---------------------
a)
Write a BNF for “SE”: a similarly simple language of “String Expressions”.
Valid ‘programs’ in this language go along the lines of pl expressions for Strings, with two exceptions:
1. Only digits 0,...,9 are allowed as valid characters within strings.
2. We will have two types of expressions that are not available in the pl language('string-insert' and 'number->string' type expressions). 

I used the lectures. I wrote an expression and checked that he was working on the examples and changed it as needed.
It was a little challenging because it's equired to make sure that the expression existed for each situation and because it was new material.

Time to write the solution(a+b): about 2 hours includes reading the material.

solution:(the answer should appear inside a comment block)
----------------------------------------------------------

<SE> ::= <D>                   ; (1)
         | "<D>"               ; (2)
         | <CHARACTER>         ; (3)     
         | <string>            ; (4)
         | <string-length>     ; (5)
         | <string-append>     ; (6)
         | <string-insert>     ; (7)
         | <number->string>    ; (8)

<string> ::=( string <C>)                           ; (9)

<string-length> ::=( string-length  "<D>" )         ; (10)

<string-append> ::= ( string-append  <STA> )        ; (11)
<STA> ::= λ                                         ; (12)
         |<string> <STA>                            ; (13)
         | "<D>" <STA>                              ; (14)
         |<number->string> <STA>                    ; (15)
         |<string-insert> <STA>                     ; (16)

<string-insert> ::= ( string-insert "<D>" <CHARACTER> <D>)  ; (17)

<number->string> ::=( number->string  <D>)                  ; (18)
                    |( number->string  <string-length>)     ; (19)

<D> ::= λ                                                   ; (20)
        | <DIGIT> <D>                                       ; (21)                                    
<DIGIT> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9             

<C> ::= λ                                                   ; (22)
        | <CHARACTER> <C>                                   ; (23) 
<CHARACTER> ::= #\0 | #\1 | #\2 | #\3 | #\4 | #\5 | #\6 | #\7 | #\8 | #\9 


b)
Add to your BNF a derivation process for 3 different SE expressions.provide a derivation tree or a series of replacements.

solution:
---------

1.
( string-append  "3" ( string #\1 #\2) "" )

<SE>		                                                         ; (6)	==>
  <string-append>                                                        ; (11)	==>
  ( string-append  <STA> )                                               ; (14)	==>
  ( string-append   "<D>" <STA> )                                        ; (13)	==>
  ( string-append   "<D>" <string> <STA> )                               ; (14)	==>
  ( string-append  "<D>" <string> "<D>" <STA> )                          ; (12)	==>
  ( string-append   "<D>" <string> "<D>" )                               ; (21)	==>
  ( string-append  "<DIGIT> <D>" <string> "<D>" )                        ; (20)	==>
  ( string-append  "<DIGIT>" <string> "<D>")                             ; (DIGIT) ==>
  ( string-append  "3" <string> "<D>")                                   ; (20) ==>
  ( string-append  "3" <string> "")                                      ; (9) ==>
  ( string-append  "3" ( string <C>) "")                                 ; (23) ==>
  ( string-append  "3" ( string <CHARACTER> <C>) "")                     ; (23) ==>
  ( string-append  "3" ( string <CHARACTER> <CHARACTER> <C>) "")         ; (22) ==>
  ( string-append  "3" ( string <CHARACTER> <CHARACTER> ) "")            ; (CHARACTER) ==>
  ( string-append  "3" ( string #\1 <CHARACTER> ) "")                    ; (CHARACTER) ==>
  ( string-append  "3" ( string #\1 #\2) "")
              
2. 
( number->string ( string-length "123" ) )

<SE>		                                                         ; (8)	==>
  <number->string>                                                       ; (19)	==>
  ( number->string  <string-length>)                                     ; (10)	==>
  ( number->string  ( string-length  "<D>" ))                            ; (21)	==>
  ( number->string  ( string-length  "<DIGIT> <D>" ))                    ; (21)	==>
  ( number->string  ( string-length  "<DIGIT> <DIGIT> <D>" ))            ; (21)	==>
  ( number->string  ( string-length  "<DIGIT> <DIGIT> <DIGIT> <D>" ))    ; (20)	==>
  ( number->string  ( string-length  "<DIGIT> <DIGIT> <DIGIT>" ))        ; (DIGIT)==>
  ( number->string  ( string-length  "1 <DIGIT> <DIGIT>" ))              ; (DIGIT)==>
  ( number->string  ( string-length  "12 <DIGIT>" ))                     ; (DIGIT)==>
  ( number->string  ( string-length  "123" )

3.
( string-insert "6666" #\7 88 )

<SE>		                                                                          ; (7)	==>
  <string-insert>                                                                         ; (17)==>
 ( string-insert "<D>" <CHARACTER> <D>)                                                   ; (21)==>
 ( string-insert  "<DIGIT> <D>" <CHARACTER> <D>)                                          ; (21)==>
 ( string-insert  "<DIGIT> <DIGIT> <D>" <CHARACTER> <D>)                                  ; (21	==>
 ( string-insert  "<DIGIT> <DIGIT> <DIGIT> <D>" <CHARACTER> <D>)                          ; (21)==>
 ( string-insert  "<DIGIT> <DIGIT> <DIGIT> <DIGIT> <D>" <CHARACTER> <D>)                  ; (20)==>
 ( string-insert  "<DIGIT> <DIGIT> <DIGIT> <DIGIT>" <CHARACTER> <D>)                      ; (21)==>
 ( string-insert  "<DIGIT> <DIGIT> <DIGIT> <DIGIT>" <CHARACTER> <DIGIT> <D>)              ; (21)==>
 ( string-insert  "<DIGIT> <DIGIT> <DIGIT> <DIGIT>" <CHARACTER> <DIGIT><DIGIT> <D>)       ; (20)==>
 ( string-insert  "<DIGIT> <DIGIT> <DIGIT> <DIGIT>" <CHARACTER> <DIGIT><DIGIT>)           ; (DIGIT)==>
 ( string-insert  "6 <DIGIT> <DIGIT> <DIGIT>" <CHARACTER> <DIGIT><DIGIT>)                 ; (DIGIT)==>
 ( string-insert  "66<DIGIT> <DIGIT>" <CHARACTER> <DIGIT><DIGIT>)                         ; (DIGIT)==>
 ( string-insert  "666<DIGIT>" <CHARACTER> <DIGIT><DIGIT>)                                ; (DIGIT)==>
 ( string-insert  "6666" <CHARACTER> <DIGIT><DIGIT>)                                      ; (DIGIT)==>
 ( string-insert  "6666" <CHARACTER> 8<DIGIT>)                                            ; (DIGIT)==>
 ( string-insert  "6666" <CHARACTER> 88)                                                  ; (CHARACTER)	==>
 ( string-insert  "6666" #\7 88)
                             
|#


#|
question 2- Accommodating Memory Operations
-------------------------------------------
1)
{* {+ {set 1} {set 2}} get}
Describe the problem that is demonstrated by this, and suggest a feature to add to the MAE semantics that will solve it.

Time to write the solution(1): about 15 minutes.

solution:
----------
The problem is that this expression can be derived in multiple ways (i.e., have multiple derivation trees).
this is not deterministic. It is because we do not know the last value stored in the memory cell (1\2) So you can get two different trees here.

To solve this problem we can add to the MAE the feature that is the last (most right) set that was written, Its value will be stored in the memory cell.
|#

#|

2)
Write a new MAE grammar that derives a programs which a computation is a non-empty
sequence of sub-computations, each one gets stored in the memory and
is available for the following sub-computation, except for the last one.

Add to your BNF a derivation process for 3 different MAE expressions.

After question 1 it was not a difficult question to implement but it took me time to understand what I was asked to do.
alse here i wrote an expression and checked that he was working on the examples and changed it as needed. 

Time to write the solution(2): about one hour.

solution:
---------

<MAE> ::= {seq <SEQ> }                          ; (1)                 

<SEQ> ::={set <num>}<SEQUENCE>                  ; (2)
         |{set <AE>}<SEQUENCE>                  ; (3)
         |<AE><SEQUENCE>                        ; (4)
         |<AE>                                  ; (5)

<SEQUENCE> ::=<AEG>                             ; (6)
              |<SET><SEQUENCE>                  ; (7)
              |<AEG><SEQUENCE>                  ; (8)

<SET> ::= {set <num>}                           ; (9)
         |{set <AEG>}                           ; (10)

<AE> ::=   { + <AEN> <AEN> }                    ; (11)
         | { - <AEN> <AEN> }                    ; (12)
         | { * <AEN> <AEN> }                    ; (13)
         | { / <AEN> <AEN> }                    ; (14)

<AEN> ::=<num>                                  ; (15)
         |<AE>                                  ; (16)
                             
<AEG> ::=  { + <AEGN> <AEGN> }                  ; (17)
         | { - <AEGN> <AEGN> }                  ; (18)
         | { * <AEGN> <AEGN> }                  ; (19)
         | { / <AEGN> <AEGN> }                  ; (20)
         | get                                  ; (21)

<AEGN> ::=<num>                                 ; (22)
         |<AEG>                                 ; (23)


1.
{seq {set {+ 20 648}}
 {set {* get get}}
 {/ get 155 }}

<MAE>		                                                         ; (1)	==>
  {seq <SEQ> }                                                           ; (3)	==>
  {seq {set <AE>}<SEQUENCE> }                                            ; (11)	==>                                          
  {seq {set { + <AEN> <AEN> }}<SEQUENCE> }                               ; (15)	==>   
  {seq {set { + 20 <AEN> }}<SEQUENCE> }                                  ; (15)	==>   
  {seq {set { + 20 648 }}<SEQUENCE> }                                    ; (7)	==>   
  {seq {set { + 20 648 }}<SET><SEQUENCE> }                               ; (10)	==>
  {seq {set { + 20 648 }}{set <AEG>}<SEQUENCE> }                         ; (19)	==>
  {seq {set { + 20 648 }}{set { * <AEGN> <AEGN> }}<SEQUENCE> }           ; (23)	==>
  {seq {set { + 20 648 }}{set { * <AEG> <AEGN> }}<SEQUENCE> }            ; (23)	==>
  {seq {set { + 20 648 }}{set { * <AEG> <AEG> }}<SEQUENCE> }             ; (21)	==>
  {seq {set { + 20 648 }}{set { * get <AEG> }}<SEQUENCE> }               ; (21)	==>
  {seq {set { + 20 648 }}{set { * get get }}<SEQUENCE> }                 ; (6)	==>
  {seq {set { + 20 648 }}{set { * get get }}<AEG> }                      ; (20)	==>
  {seq {set { + 20 648 }}{set { * get get }}{ / <AEGN> <AEGN> } }        ; (23)	==>
  {seq {set { + 20 648 }}{set { * get get }}{ / <AEG> <AEGN> } }         ; (21)	==>
  {seq {set { + 20 648 }}{set { * get get }}{ / get <AEGN> } }           ; (22)	==>
  {seq {set { + 20 648 }}{set { * get get }}{/ get 155 }}
    

2.
{seq {set {- 64 89}}{+ get 20}}

<MAE>		                                                         ; (1)	==>
  {seq <SEQ> }                                                           ; (3)	==>
  {seq {set <AE>}<SEQUENCE> }                                            ; (12)	==>                                          
  {seq {set { - <AEN> <AEN> }}<SEQUENCE> }                               ; (15)	==>   
  {seq {set { - 64 <AEN> }}<SEQUENCE> }                                  ; (15)	==>   
  {seq {set { - 64 89 }}<SEQUENCE> }                                     ; (6)	==>   
  {seq {set { - 64 89 }}<AEG> }                                          ; (17)	==>
  {seq {set { - 64 89 }}{ + <AEGN> <AEGN> } }                            ; (23)	==>
  {seq {set { - 64 89 }}{ + <AEG> <AEGN> } }                             ; (21)	==>
  {seq {set { - 64 89 }}{ + get <AEGN> } }                               ; (22)	==>
  {seq {set {- 64 89}}  {+ get 20}}

3.
{seq {set {* 20 64}}
 {set {+ get 89}}
 {- 155 get }}

<MAE>		                                                         ; (1)	==>
  {seq <SEQ> }                                                           ; (3)	==>
  {seq {set <AE>}<SEQUENCE> }                                            ; (13)	==>                                          
  {seq {set { * <AEN> <AEN> }}<SEQUENCE> }                               ; (15)	==>   
  {seq {set { * 20 <AEN> }}<SEQUENCE> }                                  ; (15)	==>   
  {seq {set { * 20 64}}<SEQUENCE> }                                      ; (7)	==>   
  {seq {set { * 20 64}}<SET><SEQUENCE> }                                 ; (10)	==>
  {seq {set { * 20 64}}{set <AEG>}<SEQUENCE> }                           ; (17)	==>
  {seq {set { * 20 64}}{set { + <AEGN> <AEGN> }}<SEQUENCE> }             ; (23)	==>
  {seq {set { * 20 64}}{set { + <AEG> <AEGN> }}<SEQUENCE> }              ; (21)	==>
  {seq {set { * 20 64}}{set { + get <AEGN> }}<SEQUENCE> }                ; (22)	==>
  {seq {set { * 20 64}}{set { + get 89 }}<SEQUENCE> }                    ; (6)	==>
  {seq {set { * 20 64}}{set { + get 89 }}<AEG>  }                        ; (18)	==>
  {seq {set { * 20 64}}{set { + get 89 }} { - <AEGN> <AEGN> }  }         ; (22)	==>
  {seq {set { * 20 64}}{set { + get 89 }} { - 155 <AEGN> }  }            ; (23)	==>
  {seq {set { * 20 64}}{set { + get 89 }} { - 155 <AEG> }  }             ; (21)	==> 
  {seq {set {* 20 64}}{set {+ get 89}}{- 155 get }}

|#

#|
question 3 - Higher Order Functions
-----------------------------------
Write a sum-of-squares function which takes a list of numbers as input, and produces a number which is the sum
of the squares of all of the numbers in the list.

It was easy question but I got stuck on it because i should add "untyped".
i was helped the site: https://stackoverflow.com/questions/22560573/how-to-do-square-in-racket


Time to write the solution: about one hour.

solution:
---------
|#

(: sum-of-squares : (Listof  Number) -> Number)
(define (sum-of-squares mylist)
  (foldl +  0 (my-square mylist)))

(: my-square : (Listof  Number) -> (Listof  Number))
(define (my-square list)
  (map square list)
  )

(: square : Number -> Number)
(define (square x)(* x x) )

;;tests

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(0 2 0)) => 4)
(test (sum-of-squares '(3 3 3)) => 27)
(test (sum-of-squares '(-1 2 3)) => 14)
(test (sum-of-squares '(1 -2 3)) => 14)
(test (sum-of-squares '(1 -2 -3)) => 14)

#|
question 4 - PAE (and more H.O. functions)
------------------------------------------
a)
Write a function that takes as arguments a list of k numbers a_0, ... , a_k-1 and returns as output a function.
The returned function takes a number x0 and return the value of the polynomial a_0 ⋅ x^0 + ⋯ + a_k-1 ⋅ x^n-1 at x_0.

This was a challenging question because it included a form of writing that I did not know
and demanded an understanding of the given code and the lecturer's intent.
I used my friend to figure out how the code should work and then I wrote the code alone.

Time to write the solution: about two hour.

solution:
---------
|#


(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
 (: poly : (Listof Number) Number Integer Number -> Number)
 (define (poly argsL x power accum)   ;a recursive function wich calculate the polynom
 (if(null? argsL)
       accum
       (poly (rest argsL) x (+ power 1)(+ accum (* (first argsL) (expt x power))))))
 (: polyX : Number -> Number)
 (define (polyX x) ; a function that call to poly with accum=0 and power=0
 (poly coeffs x 0 0))
 polyX)

;;tests

(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) =>(+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5(expt 0 3))))
(test (p2345 4) =>(+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5(expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4(expt 11 2)) (* 5 (expt 11 3))))
(test (p2345 -11) => (+ (* 2 (expt -11 0)) (* 3 (expt -11 1)) (* 4(expt -11 2)) (* 5 (expt -11 3))))

(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6(expt 11 2))))
  
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)

(define p-53-6 (createPolynomial '(-5 3 -6)))
(test (p-53-6 11) => (+ (* -5 (expt 11 0)) (* 3 (expt 11 1)) (* -6(expt 11 2))))
(test (p-53-6 -1) => (+ (* -5 (expt -1 0)) (* 3 (expt -1 1)) (* -6(expt -1 2))))

#|
b)
define a language PLANG that supports evaluating a polynomial on a sequence of points (numbers).
Write the parser for the new language.
Write the evaluation process. In order to leave the AE eval
unchanged we wrap it with an eval-poly function.

This question was ok. b_ii help me to understend how to write b_i.
For b_ii it is necessary to review and understand the code, which I did with the help of the lectures.
also the code itself in b_ii help me understend what sould i write.
b_iii was alse about review and understand the code.

Time to write the solution: about three hours.

solution:
---------

 <PLANG> ::={{poly <AEs>} {<AEs>}}

 <AEs> ::==<AE> <AEs>
           | <AE> 

 <AE> ::=<num>
 | { + <AE> <AE> }
 | { - <AE> <AE> }
 | { * <AE> <AE> }
 | { / <AE> <AE> }
|#

;;b_ii
;;----

;;define te PLANG and the AE
(define-type PLANG
 [Poly (Listof AE)(Listof AE)])

 (define-type AE
 [Num Number]
 [Add AE AE]
 [Sub AE AE]
 [Mul AE AE]
 [Div AE AE])

 (: parse-sexpr : Sexpr -> AE)
 ;; to convert s-expressions into AEs
 (define (parse-sexpr sexpr)
 (match sexpr
 [(number: n) (Num n)]
 [(list '+ lhs rhs) (Add (parse-sexpr lhs)(parse-sexpr rhs))]
 [(list '- lhs rhs) (Sub (parse-sexpr lhs)(parse-sexpr rhs))]
 [(list '* lhs rhs) (Mul (parse-sexpr lhs)(parse-sexpr rhs))]
 [(list '/ lhs rhs) (Div (parse-sexpr lhs)(parse-sexpr rhs))]
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 

 (: parse : String -> PLANG)
 ;; parses a string containing a PLANG expression to a PLANG AST
 (define (parse str)
  (let ([code (string->sexpr str)])
    (match code
      [(list (list 'poly) rest) (error 'parse "at least one coefficient is
                      required in ~s" code)]
      [(list (list 'poly var1 var2 ...) '()) (error 'parse "at least one point is
                      required in ~s" code)]
      [(list (list 'poly var1 var2 ...) (list var3 var4 ...)) (Poly (map parse-sexpr (cons var1 var2)) (map parse-sexpr (cons var3 var4)))]
      [else (error 'parse-sexpr "bad syntax in ~s" code)]
   )))

;;tests
(test (parse "{{poly 1 2 3} {1 2 3}}")
      => (Poly (list (Num 1) (Num 2) (Num 3))
               (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly } {1 2} }")
      =error> "parse: at least one coefficient is
                      required in ((poly) (1 2))")
(test (parse "{{poly 1 2} {} }")
      =error> "parse: at least one point is
                      required in ((poly 1 2) ())")
(test (parse "{{+ 1 2} {1 2} }") =error> "bad syntax in ((+ 1 2) (1 2))")
(test (parse "{{poly c} {b a} }")=error> "bad syntax in c")


;;b_iii
;;----

(: eval : AE -> Number);;Signature for the method
;; evaluates AE expressions to numbers
 (define (eval expr)
 (cases expr
 [(Num n) n]
 [(Add l r) (+ (eval l) (eval r))]
 [(Sub l r) (- (eval l) (eval r))]
 [(Mul l r) (* (eval l) (eval r))]
 [(Div l r) (/ (eval l) (eval r))]))

;; the method eval-poly returns list of numbers by the given tets
 (: eval-poly : PLANG ->  (Listof Number) )
 (define (eval-poly p-expr)
  (cases p-expr
    [(Poly l1 l2) (map (createPolynomial (map eval l1)) (map eval l2))]))

;;the method run
 (: run : String -> (Listof Number))
 ;; evaluate a FLANG program contained in a string
 (define (run str)
 (eval-poly (parse str)))

;;tests

(test (run "{{poly 1 2 3} {1 2 3}}")=> '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}")=> '(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}")=> '(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}")=> '(4/5 4/5 4/5))
(test (run "{{poly 2 3} {4}}")=> '(14))
(test (run "{{poly 1 1 0} {-1 3 3}}")=> '(0 4 4))
(test (run "{{poly {+ 1 0} 1 0} {-1 3 {* 3 1}}}")=> '(0 4 4))
(test (run "{{poly {/ 1 1} 2 3} {1 2 {- 3 0}}}")=> '(6 17 34))

