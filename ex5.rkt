#lang pl

#|
1. Re-Implementing the ROL language in the Environment Model 
------------------------------------------------------------
Re-implement the language “ROL” in the environment model.

It was an easy question.
I copied the code from the previous task and changed the code using the code in the language “FLANG” in the environment model.

Time to write the solution: About an half hour. 
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
 [Id Symbol]
 [With Symbol RegE RegE]
 [Bool Boolean]
 [Geq RegE RegE]
 [Maj RegE]
 [If RegE RegE RegE]
 [Fun  Symbol RegE]
 [Call RegE RegE])


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
[(list 'call fun arg) (Call (parse-sexpr-RegL fun reg-len) (parse-sexpr-RegL arg reg-len))]
[(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr-RegL body reg-len))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
[else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
 (parse-sexpr (string->sexpr str)))

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
[RegV  Bit-List]
[bool Boolean]
[fun Symbol RegE ENV])

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv)(error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))

 (: eval : RegE ENV -> VAL)
 ;; evaluates RegE expressions by reducing them to bit-lists
(define (eval expr env)
 (cases expr
 [(Reg bl) (RegV  bl)]
 [(Bool b)(bool b)]
 [(And r l)(reg-arith-op bit-and (eval r env) (eval l env))]
 [(Or r l)(reg-arith-op bit-or (eval r env) (eval l env))]
 [(Shl l) (RegV  (shift-left (RegV->bit-list (eval l env))))]
 [(Id id)(lookup id env)]
 [(With bound-id named-expr bound-body)
     (eval bound-body
           (Extend bound-id (eval named-expr env) env))]
 [(Geq r l)(bool (geq-bitlists? (RegV->bit-list (eval r env)) (RegV->bit-list (eval l env))))]
 [(Maj l)(bool (majority? (RegV->bit-list (eval l env))))]
 [(If con res els) (if (cases (eval con env)
                         [(bool b) b]
                         [else #t]) (eval res env) (eval els env))]

 [(Fun bound-id bound-body) (fun bound-id bound-body env)]
 [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(fun bound-id bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env) f-env))]
         [else (error 'eval "`call' expects a function, got: ~s"
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

(: reg-arith-op : (BIT BIT -> BIT) VAL VAL -> VAL)
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

(: RegV->bit-list : VAL -> Bit-List)
;; extract a bit-list from VAL type
(define (RegV->bit-list res)
  (cases res
    [(RegV bl) bl]
    [else (error RegV->bit-list "run must return a bit-list")]))


(: run : String -> Bit-List)
 ;; evaluate a ROL program contained in a string
 ;; we will not allow to return a boolean type

(define (run command)
(RegV->bit-list (eval(parse command) (EmptyEnv))))


;;tests

(test (run "{ reg-len = 3 {with {identity {fun {x} x}} {with {foo {fun {x} {or x {1 1 0}}}} {call {call identity foo} {0 1 0}}}}}") => '(1 1 0))
(test (run "{ reg-len = 3 {with {x {0 0 1}} {with {f {fun {y} {and x y}}} {with {x {0 0 0}} {call f {1 1 1}}}}}}") => '(0 0 1))
(test (run "{ reg-len = 4 {with {foo {fun {z} {if {maj? z} z {shl z}}}} {call foo {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}}}") => '(0 1 1 1))
(test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (run "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))
(test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (run "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))
(test (run "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => '(1 0 1 1))
(test (run "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => '(1 0))
(test (run "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}") =error> "no binding for y")
(test (run "{ reg-len = 2 { with {x { or {and {shl {1 0}} {1 0}} {1 0}}} {shl x}}}") => '(0 1))
(test (run "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in (0 1 1)")
(test (run "{ reg-len = 0 {}}") =error> "Register length must be at least 1")
(test (run "{ reg-len = 3 {if {geq? {1 0 1} {1 1 1}} {0 0 1} {1 1 0}}}") => '(1 1 0))
(test (run "{ reg-len = 4 {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}") => '(0 1 1 1))
(test (run "{ reg-len = 4 {if false {shl {1 0 1 1}} {1 1 0 1}}}") => '(1 1 0 1))
(test (run "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{if false {1 0} {0 1}}}}}")=>'(0 1))
(test (run "{ reg-len = 1 {if {geq? {0}{0}}{0} {0}}}") =>'(0))

#|
2."with" as a Syntactic Sugar 
-----------------------------
Changing the implementation of the eval function.
Specifically, the way it handles the With variant.
Rather than directly eval such an AST, you are to translate it into a Call tree (also using Fun)
and evaluating it as that.

I had trouble understanding what to do.
After i understend it was not so complicated
I copied the importent function from question 1,
and added for each name of the function the letter b to avoid errors.
Then I changed the function eval_b.

Time to write the solution: About an hour and a half include understending.

|#



 (: eval_b : RegE ENV -> VAL)
 ;; evaluates RegE expressions by reducing them to bit-lists
(define (eval_b expr env)
 (cases expr
 [(Reg bl) (RegV  bl)]
 [(Bool b)(bool b)]
 [(And r l)(reg-arith-op bit-and (eval_b r env) (eval_b l env))]
 [(Or r l)(reg-arith-op bit-or (eval_b r env) (eval_b l env))]
 [(Shl l) (RegV  (shift-left (RegV->bit-list (eval_b l env))))]
 [(Id id)(lookup id env)]
 [(With bound-id named-expr bound-body)
     (eval_b (Call(Fun bound-id bound-body) named-expr) env)]
 [(Geq r l)(bool (geq-bitlists? (RegV->bit-list (eval_b r env)) (RegV->bit-list (eval_b l env))))]
 [(Maj l)(bool (majority? (RegV->bit-list (eval_b l env))))]
 [(If con res els) (if (cases (eval_b con env)
                         [(bool b) b]
                         [else #t]) (eval_b res env) (eval_b els env))]
 [(Fun bound-id bound-body) (fun bound-id bound-body env)]
 [(Call fun-expr arg-expr)
     (let ([fval (eval_b fun-expr env)])
       (cases fval
         [(fun bound-id bound-body f-env)
          (eval_b bound-body
                (Extend bound-id (eval_b arg-expr env) f-env))]
         [else (error 'eval_b "`call' expects a function, got: ~s"
                      fval)]))]
    ))


(: run_b : String -> Bit-List)
 ;; evaluate a ROL program contained in a string
 ;; we will not allow to return a boolean type

(define (run_b command)
(RegV->bit-list (eval_b(parse command) (EmptyEnv))))


;;tests

(test (run_b "{ reg-len = 3 {with {identity {fun {x} x}} {with {foo {fun {x} {or x {1 1 0}}}} {call {call identity foo} {0 1 0}}}}}") => '(1 1 0))
(test (run_b "{ reg-len = 3 {with {x {0 0 1}} {with {f {fun {y} {and x y}}} {with {x {0 0 0}} {call f {1 1 1}}}}}}") => '(0 0 1))
(test (run_b "{ reg-len = 4 {with {foo {fun {z} {if {maj? z} z {shl z}}}} {call foo {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}}}") => '(0 1 1 1))
(test (run_b "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (run_b "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (run_b "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))
(test (run_b "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (run_b "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (run_b "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))
(test (run_b "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => '(1 0 1 1))
(test (run_b "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => '(1 0))
(test (run_b "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}") =error> "no binding for y")
(test (run_b "{ reg-len = 2 { with {x { or {and {shl {1 0}} {1 0}} {1 0}}} {shl x}}}") => '(0 1))
(test (run_b "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in (0 1 1)")
(test (run_b "{ reg-len = 0 {}}") =error> "Register length must be at least 1")
(test (run_b "{ reg-len = 3 {if {geq? {1 0 1} {1 1 1}} {0 0 1} {1 1 0}}}") => '(1 1 0))
(test (run_b "{ reg-len = 4 {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}") => '(0 1 1 1))
(test (run_b "{ reg-len = 4 {if false {shl {1 0 1 1}} {1 1 0 1}}}") => '(1 1 0 1))
(test (run_b "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{if false {1 0} {0 1}}}}}")=>'(0 1))
(test (run_b "{ reg-len = 1 {if {geq? {0}{0}}{0} {0}}}") =>'(0))




