#lang pl

#|
Question 1
----------
Write a function that consumes five strings
and return the first string that contains the string "pl" as a prefix, if one such exists, and returns #f otherwise.

solution
--------
Go through the strings from the very first to the last and check whether the string contains the string "pl" as a prefix.
When such a string is found, we will return it.
If we arrived recently and no such string was found we would return #f.

This question was not difficult for me. But it took me time to go through the material and learn the language and the syntax.
I was mainly assisted by presentations from the exercises.
I also used the site https://docs.racket-lang.org/reference/strings.html wich was very useful and taught me how to use substring,string-ref, stringlength and string=?.
At the first I solved the problem with string-ref but when I realized that substring would save me the code length, I changed it.

Time to write the solution: about an hour includes reading the material.
|#

(: plPrefixContained : String String String String String -> Any) 
(define(plPrefixContained s1 s2 s3 s4 s5)
  (cond
    [(and (> (string-length s1)  1) (string=? "pl" (substring s1 0 2))) s1]
    [(and (> (string-length s2)  1) (string=? "pl" (substring s2 0 2))) s2]
    [(and (> (string-length s3)  1) (string=? "pl" (substring s3 0 2))) s3]
    [(and (> (string-length s4)  1) (string=? "pl" (substring s4 0 2))) s4]
    [(and (> (string-length s5)  1) (string=? "pl" (substring s5 0 2))) s5]
    [else #f]
  )
 )

;; Test cases

(test (plPrefixContained "pll" "opl" "plll" "pllu" "lol") =>"pll" )
(test (plPrefixContained "yypl" "plt" "yyy" "ppl" "pl") =>"plt" )
(test (plPrefixContained "aaa" "bbb" "pl" "yyy" "plg") => "pl")
(test (plPrefixContained "p" "l" "lpp" "ploki" "lol") =>"ploki" )
(test (plPrefixContained "hjj" "pp" "ll" "lpl" "pllp") =>"pllp" )   
(test (plPrefixContained "yypl" "opl" "lpTT" "lpl" "lol") => false)

#|
Question 2.a
------------
Write a function that consumes a list (may contain elements of any type) and returns the longest string that is an
element of this list, and #f if no such string exists.

solution
--------
Write a tail-recursion function wich call the help function "longestString-helper".
The longestString-helper function will receive the list without the elements examined and the longest string until now.
It will check whether the next one is a longer string. If it does send it otherwise leave the current string.

Also in this question I was assisted by presentations from the exercises.
And I used the same site https://docs.racket-lang.org/reference/strings.html to know how determine whether an element is a string and what is the length.
This question was more challenging than Question 1 but required less reading of material.

Time to write the solution:about an hour.
|#

(: longestString : (Listof Any)-> Any) 
(define(longestString list)
  (: longestString-helper : Any (Listof Any)-> Any) 
  (define(longestString-helper s list)
    (cond
      [(and(null? list)(string? s)) s]
      [(and(null? list)(not(string? s))) #f]
      [(and (string? (first list)) (string? s) (> (string-length (first list)) (string-length s))) (longestString-helper (first list)(rest list))]
      [(and (string? (first list)) (not(string? s))) (longestString-helper (first list)(rest list))]
      [else (longestString-helper s (rest list))]
     )
   )
  (longestString-helper 0 list)
 )

;; Test cases

(test (longestString '(34 uuu 90)) => false)
(test (longestString '(uu 56 oooo "r" "rRR" "TTT")) => "rRR")
(test (longestString '(34 uuu "90")) => "90")
(test (longestString '()) => false)
(test (longestString '(12 "hhh" "6666666666666")) => "6666666666666")
(test (longestString '("ggg" "hhhh" "ppppp" 9999999)) => "ppppp")

#|
Question 2.b
------------
Write a function that consumes a list of lists (where the type of the elements in the inner lists may be any type).
The function returns a list of lists – such that for each inner list lst (in the original list) the following is done:
1. If lst contains at least one string, then lst is replaced with a list of size two, containing the shortest string and the longest string in lst.
2. Otherwise lst is replaced with a null.

solution
--------
My solution was to use the function from the previous section, and to add anouther function that returns the shortest string.
And now for each list we will activate the function from the previous section and if it is returned false we replace the list with a null,
otherwise we will get the longest and the shortest strings from the functions longestString and shortestString.

It was a nice question but I got stuck for a long time for calling the variable a name of a function "(list)".
my friends helped me notice this after I turned to him when I not understand for a long time what was wrong with the order (list min max)
in the function updateList.
I used the site https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29
to learn about the map function on lists and lambda.

Time to write the solution:about 3 hours
|#

#|
shortestString
\\\\\\\\\\\\\\
function that consumes a list (may contain elements of any type) and returns the shortest string that is an
element of this list, and #f if no such string exists.
Similar to the longestString function with few small changes!
|#

(: shortestString : (Listof Any)-> Any) 
(define(shortestString list)
  (: shortestString-helper : Any (Listof Any)-> Any) 
  (define(shortestString-helper s list)
    (cond
      [(and(null? list)(string? s)) s]
      [(and(null? list)(not(string? s))) #f]
      [(and (string? (first list)) (string? s) (< (string-length (first list)) (string-length s))) (shortestString-helper (first list)(rest list))]
      [(and (string? (first list)) (not(string? s))) (shortestString-helper (first list)(rest list))]
      [else (shortestString-helper s (rest list))]
     )
   )
  (shortestString-helper 0 list)
 )

#|
updateList
\\\\\\\\\\
Update the list to the shortest string and the longest string if they exist.
otherwise null.
|#

(: updateList : (Listof Any)-> Any) 
(define(updateList inerlist)
  (let([max (longestString inerlist)][min (shortestString inerlist)])
    (if(string? max)
       (list min max)
       null )) 
)

#|
short&long-lists
\\\\\\\\\\\\\\\\\
|#

(: short&long-lists : (Listof(Listof Any)) -> (Listof Any)) 
(define(short&long-lists list)
   (map(lambda(inerlist) (updateList inerlist)) list)
)

;; Test cases

(test (short&long-lists '((any "Benny" 10 "OP" 8) (any Benny OP (2 3)))) => '(("OP" "Benny") ()))
(test (short&long-lists '(("2 5 5" 1 "5gg" L) (v gggg "f") ())) => '(("5gg" "2 5 5") ("f" "f") ()))
(test (short&long-lists '((2 5 5 1 5gg L) (v "gggg" "f") )) => '(() ("f" "gggg") ))
(test (short&long-lists '((2 5 5 "11" "5gg" "LLLLLLL"))) => '(("11" "LLLLLLL")))
(test (short&long-lists '(())) => '(()))
 
#|
Question 3
----------
Implemention of a keyed-stack data structure.
This data tructure is a new type called KeyStack wich each element in the stack is keyed (indexed) with a symbol.
In addition, we will implement the foloowing operations:
1.empty stack EmptyKS.
2.operation Push- take as input a symbol (key), a string (value), and an existing keyed-stack and return an extended keystack.
3.operation search-stack – take as input a symbol (key) and a keyed-stack and return the first value that is keyed accordingly.
If the key does not appear in the original stack, it should return a #f value.
4.operation pop-stack –take as input a keyed-stack and return the keyed-stack without its first (keyed)
value. If the original stack was empty, it should return a #f value.

solution
--------
firt I defined the structure using the site https://docs.racket-lang.org/plait/datatypes-tutorial.html
I desided to put the EmptyKS as fields in the new type.
the structure is built from an empty stack field and a field that
contains the upper part and the stack underneath it recursively.

Time to write the solution:about 2 hours
|#

(define-type KeyStack
  [EmptyKS]
  [Element Symbol String KeyStack])

#|
Push
////
|#
(: Push : Symbol String KeyStack -> KeyStack)
(define (Push key value kS)
  (Element key value kS)
)

#|
search-stack
////////////
|#
(: search-stack : Symbol KeyStack -> Any)
(define (search-stack key kS)
  (cases kS
    [(Element symbol string keyStack)
     (if (eq? key symbol)
        string
        (search-stack key keyStack))] 
    [(EmptyKS) #f]) 
  )


#|
pop-stack
/////////
|#
(: pop-stack : KeyStack -> Any)
(define (pop-stack kS)
  (cases kS
    [(Element symbol string keyStack) keyStack]
    [(EmptyKS) #f]) 
  )


;; Test cases

(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'b (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "B")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'c (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)

#|
Question 4
----------
Add the appropriate comments for each of the functions. These comments should describe what the function
takes as input, what it outputs, what its purpose is, and how it operates.

Time to write the solution: 30 Minutes
|#

(: is-odd? : Natural -> Boolean)
#|The function is-odd? consumes a Natural number and returns a Boolean.
true if the number is odd, otherwise false.
The function check if the number is 0 and returns false if it is.
Then it is call the function is-even with x - 1.

the idea is that the function is-even do the same is just the opposite.
So the number move from one function to the other until it reaches 0.
and then the boolean value is determined.

is-odd and is-even came together.
|#
(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))

(: is-even? : Natural -> Boolean)
#|The function is-even? consumes a Natural number and returns a Boolean.
true if the number is even, otherwise false.

this function work just like the function is-odd and they dependent on each other.
|#
(define (is-even? x)
 (if (zero? x)
 true
 (is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
#|The function every? consumes a boolean method wich work on type A (pred)and a list of type A (lst), and returns a Boolean.
true if pred is true for every elements in the list, otherwise false.
if the list is null the function return true.
Then it go over the elements of the list recursuvly and check if it get true from the function pred.
if there is an element that got false from the function pred it return false.
|#
(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst)))))


(: all-even? : (Listof Natural) -> Boolean)
#|The function all-even? consumes a list of Natural number and returns a Boolean.
the boolean value that returned is the value of the function every? with the function is-even? and the list as input.
this function is an example for the usefulness of the function every?.
it return true if all the elemenets of the list are even, otherwise false.
|#
(define (all-even? lst)
 (every? is-even? lst))

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))

(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
Boolean))
#|The every2? consumes two boolean method wich one work on type A(pred1) and the other on type B(pred2)
and a two lists one of type A (lst1) and the other of type B(lst2), and returns a Boolean.
true if pred1 is true for every elements in the lst1 and pred2 is true for every elements in the lst2, otherwise false.
if the list is null the function return true.
Then it go over the elements of lst1 and lst2 recursuvly and check if it get true from function pred1 and pred2(pred1 to lst1 and pred2 to lst2).
if there is an element that got false from function that adapted to him it return false.

|#
(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists assumed to be of same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))