#lang pl 03

#|
1. Adding with expressions to the ROL language
----------------------------------------------
write the language “ROL” – a simple language for “Register Operation Expressions”.
It sould support "if" expression, and two Boolean function “geq?” and “maj?” and Boolean types.
“geq?“ return true if given two registers, the first register's (binary) value is greater or equal to the value of the second register.
“maj?” returns true if the majority of bits in the register are ones.

I used presentations from the exercises wich the "ROL" language Realized there.
I went over the given code to figure out what to add. It was not that difficult after the previous assignment.
I did not get into any difficulties.


Time to write the solution: 1 hour

|#

#| BNF for the ROL language:
 <ROL> ::= {reg-len=<num><RegE>}
 <RegE> ::= <Bits>
           |{and <RegE><RegE>}
           |{or <RegE><RegE>}
           |{shl <RegE>}
           |{with {<ID><RegE>}<RegE>}
           |{<ID>}
           |{geq? <RegE> <RegE>}
           |{maj? <RegE>}
           |{if <RegE> <RegE> <RegE>}
           | <Bool>


 <Bool> ::= true
           |false

 <Bits> ::= <bit>
           |<bit><Bits>

 <bit>::= 1
         |0
|#
;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))
;; RegE abstract syntax trees
(define-type RegE
 [Reg Bit-List]
 [And RegE RegE]
 [Or RegE RegE]
 [Shl RegE]
 [Id Symbol]
 [With Symbol RegE RegE]
 [Bool Boolean]
 [Geq RegE RegE]
 [Maj RegE]
 [If RegE RegE RegE])

;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
(cond [(null? lst) null]
[(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
[else (cons 0 (list->bit-list (rest lst)))]))

(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
(match sexpr
[(list 'reg-len'= (number: n) args)
      (if(> n 0)
        (parse-sexpr-RegL args n)
        (error 'parse-sexpr "Register length must be at least 1 ~s" sexpr))] ;; remember to make sure specified register length is at least 1
[else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegL sexpr reg-len)
(match sexpr
['true (Bool #t)]
['false (Bool #f)]
[(list (and a (or 1 0)) ... )
 (if(= reg-len (length a))
   (Reg(list->bit-list a))
   (error 'parse-sexpr "wrong number of bits in ~s" a))]
[(list 'and list1 list2)(And(parse-sexpr-RegL list1 reg-len)(parse-sexpr-RegL list2 reg-len))]
[(list 'or list1 list2)(Or(parse-sexpr-RegL list1 reg-len)(parse-sexpr-RegL list2 reg-len))]
[(list 'shl list)(Shl(parse-sexpr-RegL list reg-len))]
[(symbol: id-name)(Id id-name)]
[(cons 'with args)
 (match sexpr
   [(list 'with (list(symbol: oldName)newName)body)
    (With oldName(parse-sexpr-RegL newName reg-len)(parse-sexpr-RegL body reg-len))]
    [else (error 'parse-sexpr-RegE "bad `with' syntax in ~s" sexpr)])]
[(boolean: b)(Bool b)]
[(list 'geq? list1 list2)(Geq(parse-sexpr-RegL list1 reg-len)(parse-sexpr-RegL list2 reg-len))]
[(list 'maj? list)(Maj(parse-sexpr-RegL list reg-len))]
[(list 'if list1 list2 list3)(If(parse-sexpr-RegL list1 reg-len)(parse-sexpr-RegL list2 reg-len)(parse-sexpr-RegL list3 reg-len))]
[else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
 (parse-sexpr (string->sexpr str)))


#|
2.Supported Types
-----------------
Note that our "ROL" language allows for two types of outputs. Boolean and register.
Define a new type RES that has two variants, one for each output type.

Time to write the solution: 5 minutes

|#

(define-type RES
[RegV  Bit-List]
[bool Boolean])

#|
3. Substitutions
----------------
A "subst" function wich get register symbol and anouther register.
the function substitutes the second argument with the third argument in the
first argument, as per the rules of substitution; the resulting
expression contains no free instances of the second argument.

also here I used presentations from the exercises.
I took the code from there and made changes to suit our language.

Time to write the solution: 45 minutes.

|#

(: subst : RegE Symbol RegE -> RegE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument

(define (subst expr from to)
  (cases expr
    [(Reg bl) expr]
    [(Bool bool) expr]
    [(And l r) (And (subst l from to) (subst r from to))]
    [(Or l r) (Or (subst l from to) (subst r from to))]
    [(Shl l) (Shl (subst l from to))]
    [(Id id) (if (eq? id from) to expr)]
    [(With id l r)
     (With id
           (subst l from to)
           (if (eq? id from)
               r
               (subst r from to)))]
    [(Geq l r) (Geq (subst l from to) (subst r from to))]
    [(Maj l) (Maj (subst l from to))]
    [(If con res els) (If (subst con from to) (subst res from to) (subst els from to))]   
    ))
#|

4. Evaluation 
-------------
write a function wich evaluates RegE expressions by reducing them to bit-lists.

also here I used presentations from the exercises.
I took the code from there and made changes to suit our language.
The part of the completion was more complicated and I needed an explanation from my friend to understand what the higher-performing functions should do.


Time to write the solution:2 hours.

|#

 (: eval : RegE -> RES)
 ;; evaluates RegE expressions by reducing them to bit-lists
(define (eval expr)
 (cases expr
 [(Reg bl) (RegV  bl)]
 [(Bool b)(bool b)]
 [(And r l)(reg-arith-op bit-and (eval r) (eval l))]
 [(Or r l)(reg-arith-op bit-or (eval r) (eval l))]
 [(Shl l) (RegV  (shift-left (RegV->bit-list (eval l))))]
 [(Id id)(error 'eval "free identifier: ~s" id)]
 [(With id l r)
   (eval (subst r
                  id
                  (cases (eval l)
                    [(RegV  reg) (Reg reg)]
                    [(bool b) (Bool b)])))]  
 [(Geq r l)(bool (geq-bitlists? (RegV->bit-list (eval r)) (RegV->bit-list (eval l))))]
 [(Maj l)(bool (majority? (RegV->bit-list (eval l))))]
 [(If con res els) (if (cases (eval con)
                         [(bool b) b]
                         [else #t]) (eval res) (eval els))]
))

;; Defining functions for dealing with arithmetic operations
;; on the above types
 (: bit-and : BIT BIT -> BIT) ;; Arithmetic and
 (define(bit-and a b)
   (if(and (= a  1) (= b  1)) 1 0))

 (: bit-or : BIT BIT -> BIT) ;; Aithmetic or
 (define(bit-or a b)
  (if(and (= a  0) (= b  0)) 0 1))

(: reg-arith-op : (BIT BIT -> BIT) RES RES -> RES)
;; Consumes two registers and some binary bit operation 'op',
;; and returns the register obtained by applying op on the
;; i'th bit of both registers for all i.
(define(reg-arith-op op reg1 reg2)
  (: bit-arith-op : Bit-List Bit-List -> Bit-List)
  ;; Consumes two bit-lists and uses the binary bit operation 'op'.
  ;; It returns the bit-list obtained by applying op on the
  ;; i'th bit of both registers for all i.
  (define(bit-arith-op bl1 bl2)
    (map op bl1 bl2))
  (RegV  (bit-arith-op (RegV->bit-list reg1) (RegV->bit-list reg2))))

(: majority? : Bit-List -> Boolean)
;; Consumes a list of bits and checks whether the
;; number of 1's are at least as the number of 0's.
(define(majority? bl)
  (if (>= (foldl + 0 bl) (/ (length bl) 2)) #t #f))

(: geq-bitlists? : Bit-List Bit-List -> Boolean)
;; Consumes two bit-lists and compares them. It returns true if the
;; first bit-list is larger or equal to the second.
(define (geq-bitlists? bl1 bl2)
 (cond
    [(null? bl1) #t]
    [(eq? (first bl1) 1) (if (eq? (first bl2) 1) (geq-bitlists? (rest bl1) (rest bl2)) #t)]
    [(eq? (first bl2) 1)  #f]
    [else (geq-bitlists? (rest bl1) (rest bl2))]))

(: shift-left : Bit-List -> Bit-List)
;; Shifts left a list of bits (once)
(define(shift-left bl)
  (append (rest bl) (list (first  bl))))

(: RegV->bit-list : RES -> Bit-List)
;; extract a bit-list from RES type
(define (RegV->bit-list res)
  (cases res
    [(RegV bl) bl]
    [(bool b) (error RegV->bit-list "run must return a bit-list ~s" b)]))
#|
5. Interface 
------------
Write a run function wich wrap it all up.

Time to write the solution: 10 minuts.

|#

(: run : String -> Bit-List)
 ;; evaluate a ROL program contained in a string
 ;; we will not allow to return a boolean type

(define (run command)
(RegV->bit-list (eval(parse command))))



;; tests
 (test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
 (test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
 (test (run "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))
 (test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
 (test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
 (test (run "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))
 (test (run "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => '(1 0 1 1))
 (test (run "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => '(1 0))
 (test (run "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}") =error> "free identifier: y")
 (test (run "{ reg-len = 2 { with {x { or {and {shl {1 0}} {1 0}} {1 0}}} {shl x}}}") => '(0 1))
 (test (run "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in (0 1 1)")
 (test (run "{ reg-len = 0 {}}") =error> "Register length must be at least 1")
 (test (run "{ reg-len = 3 {if {geq? {1 0 1} {1 1 1}} {0 0 1} {1 1 0}}}") => '(1 1 0))
 (test (run "{ reg-len = 4 {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}") => '(0 1 1 1))
 (test (run "{ reg-len = 4 {if false {shl {1 0 1 1}} {1 1 0 1}}}") => '(1 1 0 1))
 (test (run "{ reg-len = prob {if false {shl {1 0 1 1}} {1 1 0 1}}}")=error> "parse-sexpr: bad syntax in (reg-len = prob (if false (shl (1 0 1 1)) (1 1 0 1)))")
 (test (run "{ reg-len = 4 {with {x prob prob}}}")=error> "parse-sexpr-RegE: bad `with' syntax in (with (x prob prob))")
 (test (run "{ reg-len = 2 {prob}}")=error> "parse-sexpr: bad syntax in (prob)")
 (test (run "{ reg-len = 2 {maj? {0 0}}}")=error> "#<procedure:RegV->bit-list>: run must return a bit-list #f")
 (test (run "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{maj? x}}}")=error> "#<procedure:RegV->bit-list>: run must return a bit-list #t")
 (test (run "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{if false {1 0} {0 1}}}}}")=>'(0 1))
 (test (run "{ reg-len = 1 {if {geq? {0}{0}}{0} {0}}}") =>'(0))

 

