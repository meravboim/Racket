#lang pl

#|
1.Adding function expressions to the ROL language 
-------------------------------------------------
Extend your ROL language to also support fun and call similarly to the FLANG language.
Make sure to implement your interpreter in the substitution model.
- Extend your BNF and Parser to Support this syntax.
- Extend the eval procedure to support first class functions.

First, I copied the code for a ROL language. Then I looked at the FLANG language and change and add as required.
I used the lectures to know what to add and where.
I had no difficulty with this question other than adjusting the additions to the code And understand what needs to be done.
I understood after being asked questions in the forum and the lecturer answered them.

Time to write the solution: About an hour and a half. 
|#


#| BNF for the extend ROL language:
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
           |{ fun {<ID>} <RegE> }
           |{ call <RegE> <RegE> }

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
 [IdFROL Symbol]
 [WithFROL Symbol RegE RegE]
 [Bool Boolean]
 [Geq RegE RegE]
 [Maj RegE]
 [If RegE RegE RegE]
 [FunFROL  Symbol RegE]
 [CallFROL RegE RegE])


;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
(cond [(null? lst) null]
[(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
[else (cons 0 (list->bit-list (rest lst)))]))

(: parse-sexprFROL : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexprFROL sexpr)
(match sexpr
[(list 'reg-len'= (number: n) args)
      (if(> n 0)
        (parse-sexpr-RegLFROL args n)
        (error 'parse-sexprFROL "Register length must be at least 1 ~s" sexpr))] ;; remember to make sure specified register length is at least 1
[else (error 'parse-sexprFROL "bad syntax in ~s" sexpr)]))

(: parse-sexpr-RegLFROL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegLFROL sexpr reg-len)
(match sexpr
['true (Bool #t)]
['false (Bool #f)]
[(list (and a (or 1 0)) ... )
 (if(= reg-len (length a))
   (Reg(list->bit-list a))
   (error 'parse-sexprFROL "wrong number of bits in ~s" a))]
[(list 'and list1 list2)(And(parse-sexpr-RegLFROL list1 reg-len)(parse-sexpr-RegLFROL list2 reg-len))]
[(list 'or list1 list2)(Or(parse-sexpr-RegLFROL list1 reg-len)(parse-sexpr-RegLFROL list2 reg-len))]
[(list 'shl list)(Shl(parse-sexpr-RegLFROL list reg-len))]
[(symbol: id-name)(IdFROL id-name)]
[(cons 'with args)
 (match sexpr
   [(list 'with (list(symbol: oldName)newName)body)
    (WithFROL oldName(parse-sexpr-RegLFROL newName reg-len)(parse-sexpr-RegLFROL body reg-len))]
    [else (error 'parse-sexpr-RegEFROL "bad `with' syntax in ~s" sexpr)])]
[(boolean: b)(Bool b)]
[(list 'geq? list1 list2)(Geq(parse-sexpr-RegLFROL list1 reg-len)(parse-sexpr-RegLFROL list2 reg-len))]
[(list 'maj? list)(Maj(parse-sexpr-RegLFROL list reg-len))]
[(list 'if list1 list2 list3)(If(parse-sexpr-RegLFROL list1 reg-len)(parse-sexpr-RegLFROL list2 reg-len)(parse-sexpr-RegLFROL list3 reg-len))]
[(list 'call fun arg) (CallFROL (parse-sexpr-RegLFROL fun reg-len) (parse-sexpr-RegLFROL arg reg-len))]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (FunFROL name (parse-sexpr-RegLFROL body reg-len))]
       [else (error 'parse-sexprFROL "bad `fun' syntax in ~s" sexpr)])]
[else (error 'parse-sexprFROL "bad syntax in ~s" sexpr)]))

(: parseFROL : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parseFROL str)
 (parse-sexprFROL (string->sexpr str)))

(define-type RES
[RegV  Bit-List]
[bool Boolean]
[fun Symbol RegE])

(: substFROL : RegE Symbol RegE -> RegE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument

(define (substFROL expr from to)
  (cases expr
    [(Reg bl) expr]
    [(Bool bool) expr]
    [(And l r) (And (substFROL l from to) (substFROL r from to))]
    [(Or l r) (Or (substFROL l from to) (substFROL r from to))]
    [(Shl l) (Shl (substFROL l from to))]
    [(IdFROL id) (if (eq? id from) to expr)]
    [(WithFROL id l r)
     (WithFROL id
           (substFROL l from to)
           (if (eq? id from)
               r
               (substFROL r from to)))]
    [(Geq l r) (Geq (substFROL l from to) (substFROL r from to))]
    [(Maj l) (Maj (substFROL l from to))]
    [(If con res els) (If (substFROL con from to) (substFROL res from to) (substFROL els from to))]
    [(CallFROL l r) (CallFROL (substFROL l from to) (substFROL r from to))]
    [(FunFROL bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (FunFROL bound-id (substFROL bound-body from to)))]
    ))

 (: evalFROL : RegE -> RES)
 ;; evaluates RegE expressions by reducing them to bit-lists
(define (evalFROL expr)
 (cases expr
 [(Reg bl) (RegV  bl)]
 [(Bool b)(bool b)]
 [(And r l)(reg-arith-op bit-and (evalFROL r) (evalFROL l))]
 [(Or r l)(reg-arith-op bit-or (evalFROL r) (evalFROL l))]
 [(Shl l) (RegV  (shift-left (RegV->bit-list (evalFROL l))))]
 [(IdFROL id)(error 'evalFROL "free identifier: ~s" id)]
 [(WithFROL id l r)
     (evalFROL (substFROL r
                          id
                         l))]  
 [(Geq r l)(bool (geq-bitlists? (RegV->bit-list (evalFROL r)) (RegV->bit-list (evalFROL l))))]
 [(Maj l)(bool (majority? (RegV->bit-list (evalFROL l))))]
 [(If con res els) (if (cases (evalFROL con)
                         [(bool b) b]
                         [else #t]) (evalFROL res) (evalFROL els))]

 [(FunFROL bound-id bound-body) (fun bound-id bound-body)]
    [(CallFROL fun-expr arg-expr)
     (let ([fval (evalFROL fun-expr)])
       (cases fval
         [(fun bound-id bound-body)
          (evalFROL (substFROL bound-body
                               bound-id
                               arg-expr))]
         [else (error 'evalFROL "`call' expects a function, got: ~s"
                      fval)]))]
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
    [else (error RegV->bit-list "run must return a bit-list")]))


(: runFROL : String -> Bit-List)
 ;; evaluate a ROL program contained in a string
 ;; we will not allow to return a boolean type

(define (runFROL command)
(RegV->bit-list (evalFROL(parseFROL command))))


;;tests

(test (runFROL "{ reg-len = 3 {with {identity {fun {x} x}} {with {foo {fun {x} {or x {1 1 0}}}} {call {call identity foo} {0 1 0}}}}}") => '(1 1 0))
(test (runFROL "{ reg-len = 3 {with {x {0 0 1}} {with {f {fun {y} {and x y}}} {with {x {0 0 0}} {call f {1 1 1}}}}}}") => '(0 0 1))
(test (runFROL "{ reg-len = 4 {with {foo {fun {z} {if {maj? z} z {shl z}}}} {call foo {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}}}") => '(0 1 1 1))
(test (runFROL "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (runFROL "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (runFROL "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))
(test (runFROL "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (runFROL "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (runFROL "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))
(test (runFROL "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => '(1 0 1 1))
(test (runFROL "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => '(1 0))
(test (runFROL "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}") =error> "free identifier: y")
(test (runFROL "{ reg-len = 2 { with {x { or {and {shl {1 0}} {1 0}} {1 0}}} {shl x}}}") => '(0 1))
(test (runFROL "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in (0 1 1)")
(test (runFROL "{ reg-len = 0 {}}") =error> "Register length must be at least 1")
(test (runFROL "{ reg-len = 3 {if {geq? {1 0 1} {1 1 1}} {0 0 1} {1 1 0}}}") => '(1 1 0))
(test (runFROL "{ reg-len = 4 {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}") => '(0 1 1 1))
(test (runFROL "{ reg-len = 4 {if false {shl {1 0 1 1}} {1 1 0 1}}}") => '(1 1 0 1))
(test (runFROL "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{if false {1 0} {0 1}}}}}")=>'(0 1))
(test (runFROL "{ reg-len = 1 {if {geq? {0}{0}}{0} {0}}}") =>'(0))

#|
2.Counting Free Instances of a Given Symbol
-------------------------------------------
Write a function countFreeSingle that takes an abstract syntax tree for the FLANG
language and a symbol, and returns the number of free instances of the given
symbol appear in the expression.

The question was a bit difficult for me to understand. I used my friend to understand it.
After I understood it, it was not so complicated.

Time to write the solution:about 1 hour.

|#

;; FLANG. Copied from assignment
;;---<<<FLANG>>>-------------------------------------------------------
 ;; The Flang interpreter – supporting both the substitution model and
 #|
 The grammar:
 <FLANG> ::= <num>
 | { + <FLANG> <FLANG> }
 | { - <FLANG> <FLANG> }
 | { * <FLANG> <FLANG> }
 | { / <FLANG> <FLANG> }
 | { with { <id> <FLANG> } <FLANG> }
 | <id>
 | { fun { <id> } <FLANG> }
 | { call <FLANG> <FLANG> }
|#
 (define-type FLANG
 [Num Number]
 [Add FLANG FLANG]
 [Sub FLANG FLANG]
 [Mul FLANG FLANG]
 [Div FLANG FLANG]
 [Id Symbol]
 [With Symbol FLANG FLANG]
 [Fun Symbol FLANG]
 [Call FLANG FLANG])
 (: parse-sexpr : Sexpr -> FLANG)
 ;; to convert s-expressions into FLANGs
 (define (parse-sexpr sexpr)
 (match sexpr
 [(number: n) (Num n)]
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
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
 (: parse : String -> FLANG)
 ;; parses a string containing a FLANG expression to a FLANG AST
 (define (parse str)
 (parse-sexpr (string->sexpr str)))
;;;;;; the evaluation part for the substitution model
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
 (Fun bound-id (subst bound-body from to)))]))
 (: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
 ;; gets a Racket numeric binary operator, and uses it within a FLANG
 ;; `Num' wrapper
 (define (arith-op op expr1 expr2)
 (: Num->number : FLANG -> Number)
 (define (Num->number e)
 (cases e
 [(Num n) n]
 [else (error 'arith-op "expects a number, got: ~s" e)]))
 (Num (op (Num->number expr1) (Num->number expr2))))
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
 (let ([fval (eval fun-expr)])
 (cases fval
 [(Fun bound-id bound-body)
 (eval (subst bound-body
 bound-id
(eval arg-expr)))]
 [else (error 'eval "`call' expects a function, got: ~s"
 fval)]))]))
 (: run : String -> Number)
 ;; evaluate a FLANG program contained in a string
 (define (run str)
 (let ([result (eval (parse str))])
 (cases result
 [(Num n) n]
 [else (error 'run
 "evaluation returned a non-number: ~s" result)])))
 ;; tests
 (test (run "{call {fun {x} {+ x 1}} 4}")
 => 5)
 (test (run "{with {add3 {fun {x} {+ x 3}}}
 {call add3 1}}")
 => 4)
 (test (run "{with {add3 {fun {x} {+ x 3}}}
 {with {add1 {fun {x} {+ x 1}}}
 {with {x 3}
 {call add1 {call add3 x}}}}}")
 => 7)
 (test (run "{with {identity {fun {x} x}}
 {with {foo {fun {x} {+ x 1}}}
 {call {call identity foo} 123}}}")
 => 124)
 (test (run "{call {call {fun {x} {call x 1}}
 {fun {x} {fun {y} {+ x y}}}}
 123}")
 => 124)
;;;;;; The evaluation part for the substitution cache model
;; a type for substitution caches:
 (define-type SubstCache = (Listof (List Symbol FLANG)))
 (: empty-subst : SubstCache)
 (define empty-subst null)
 (: extend : Symbol FLANG SubstCache -> SubstCache)
 (define (extend name val sc)
 (cons (list name val) sc))
 (: lookup : Symbol SubstCache -> FLANG)
 (define (lookup name sc)
 (let ([cell (assq name sc)])
 (if cell
 (second cell)
 (error 'lookup "no binding for ~s" name))))

 (: counterx : Natural)
 (define counterx 0)
 ;;;above eval
 (: evalSC : FLANG SubstCache -> FLANG)
 ;; evaluates FLANG expressions by reducing them to expressions
 (define (evalSC expr sc)
 (set! counterx (add1 counterx))
 (if (> counterx 500)
 (error 'eval "exceeded 500 times")
 (cases expr
 [(Num n) expr]
 [(Add l r) (arith-op + (evalSC l sc) (evalSC r sc))]
 [(Sub l r) (arith-op - (evalSC l sc) (evalSC r sc))]
 [(Mul l r) (arith-op * (evalSC l sc) (evalSC r sc))]
 [(Div l r) (arith-op / (evalSC l sc) (evalSC r sc))]
 [(With bound-id named-expr bound-body)
 (evalSC bound-body
 (extend bound-id (evalSC named-expr sc) sc))]
 [(Id name) (lookup name sc)]
 [(Fun bound-id bound-body) expr]
 [(Call fun-expr arg-expr)
 (let ([fval (evalSC fun-expr sc)])
 (cases fval
 [(Fun bound-id bound-body)
 (evalSC bound-body
 (extend bound-id (evalSC arg-expr sc) sc))]
 [else (error 'evalSC "`call' expects a function, got: ~s"
 fval)]))])))
 (: runSC : String -> Number)
 ;; evaluate a FLANG program contained in a string
 (define (runSC str)
 (let ([result (evalSC (parse str) empty-subst)])
 (cases result
 [(Num n) n]
 [else (error 'runSC
 "evaluation returned a non-number: ~s" result)])))
;;---<<<END>>>-------------------------------------------------------

;; countFreeSingle function
 (: countFreeSingle : FLANG Symbol -> Natural)
 (define (countFreeSingle expr name)
  (cases expr
    [(Num number)0]
    [(Add F1 F2)(+ (countFreeSingle F1 name) (countFreeSingle F2 name))]
    [(Sub F1 F2)(+ (countFreeSingle F1 name) (countFreeSingle F2 name))]
    [(Mul F1 F2)(+ (countFreeSingle F1 name) (countFreeSingle F2 name))]
    [(Div F1 F2)(+ (countFreeSingle F1 name) (countFreeSingle F2 name))]
    [(Id id) (if (eq? id name) 1 0)]
    [(With id  F1 F2)(if (eq? id name) (countFreeSingle F1 name)
                                               (+ (countFreeSingle F2 name) (countFreeSingle F1 name)))]
    [(Fun  id F)(if (eq? id name) 0 (countFreeSingle F name))]
    [(Call  F1 F2)(+ (countFreeSingle F1 name) (countFreeSingle F2 name))]
    )
  )


 (: CFSingle : String Symbol -> Natural)
 (define (CFSingle expr name)
 (countFreeSingle (parse expr) name))

(test (CFSingle "{+ r r}" 'r) => 2)
(test (CFSingle "{fun {r} {+ r e}}" 'e) => 1)
(test (CFSingle "{fun {r} {+ r e}}" 'r) => 0)
(test (CFSingle "{call {fun {r} {+ r e}} {with {e {+ e r}} {fun {x} {+ e r}}}}" 'r) => 2)
(test (CFSingle "{call {fun {r} {+ r e}} {with {e {+ e r}} {fun {x} {+ e r}}}}"  'e) => 2)



#|
a.An interesting test case
--------------------------
Consider the following FLANG code:
"{with {foo {fun {y} {+ x y}}}
 {with {x 4}
 {call foo 3}}}"

...................................................................
What happens when you run the FLANG interpreter (substitution model)
on this code?
(run "{with {foo {fun {y} {+ x y}}}
 {with {x 4}
 {call foo 3}}}")

Answer:
-------
When I run this code it output the number 7.
When we run this code the program Sees the variable with and performs the operations that are mapped to the variable with.
Therefore, according to the code we get {with {x 4}{call {fun {y} {+ x y}} 3}}}.
Again, the program sees the variable with, so it executes the correct operations again and says x is 4.
and we get {call {fun {y} {+ 4 y}} 3}}.
then we get {+ 4 3} which is 7.
....................................................................
What happens when you run the following code?
(CFSingle "{with {foo {fun {y} {+ x y}}}
 {with {x 4}
 {call foo 3}}}" 'x)

Answer:
-------
When I run this code it output the number 1.
It's actually the same phrase just that here CFSingle is activated.
I think the change in the value that returns results from the fact that in CFSingle The x is also obtained separately, and not as part of the expression,
in contrast to the function run.
But after you enter to with then x is no longer free.
...................................................................
|#

#|
3.Static versus Dynamic Scoping (recursion)
-------------------------------------------
The code loop should be such that in the substitution model a "no binding" error
message is given, and in the substitution-cache model, eval should go into an
infinite loop.

This question was complicated. Mostly I got entangled in understanding it,
But also to write it down.
On this question, I was assisted by my friend.


Time to write the solution:about 3 hours until I was helped my friend.
|#

(define loop "{with {Merav {fun {x} {call f x}}} {with {f {fun {x} {call Merav x}}} {call Merav f}}}")

;;tests

(test (runSC loop) =error> "exceeded 500 times") ;; subst-cache model
(test (run loop) =error> "free identifier: f") ;; substitution model
