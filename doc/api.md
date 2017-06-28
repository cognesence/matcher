# API Documentation

## Contents

### Introduction

+ [Getting Started](#getting-started)
+ [A Brief Tutorial](#a-brief-tutorial)
    - [Switching and Specialisation](#switching-and-specialisation)
    - [Searching and Selection](#searching-and-selection)
    - [Iteration and Collection](#iteration-and-collection)

### API Contents

+ [General Principles](#general-principles)
    - [Patterns](#patterns)
    - [Matcher Variables](#matcher-variables)
+ Matcher Let
    - mlet
+ Function Definition
    - defmatch
    - mfn
+ Matched Output
    - mout
    - ?
+ Matcher Namespace
    - mvars
    - with-mvars
    - :it
+ Searching and Selection
    - mfind
    - mfind\*
    - mfor
    - mfor\*
+ Conditional Forms
    - mcond
    - mif
    - massert
+ Advanced Features
    - Predicates in Patterns
    - :not and :guard
+ Examples
    - Searching Sets of Tuples
    - Applying Rules
    - Applying State Changing Operators
    - Writing Dispatchers
    - Specifying a Grammar

## Getting Started

```
TODO: Leiningen!
```

## A Brief Tutorial

The most primitive form of matcher expression provided for general use is `mlet` (`matcher-let`), it is structured as
follows:

```clojure
(mlet [ pattern datum ]
  ...body...
  )
```

`mlet` operates as follows: if the pattern matches the datum, binding any matcher variables as part of the matching
process then `mlet` evaluates its body in the context of these bindings. If the pattern and datum do not match, `mlet`
returns `nil`.

Match variables are prefixed with a `?` (or `??` - see later), in the following example, the pattern `(?x ?y ?z)`
matches the datum `(cat dog bat)` binding match variables `x`, `y`, `z` to `'cat`, `'dog`, `'bat` respectively and the
expression `(? y)` in the body of mlet retrieves the value of the match variable `y` from matcher name space.

```clojure
(mlet ['(?x ?y ?z) '(cat dog bat)]
  (? y))
; → dog
```

`mout` (`matcher-out`) is a convenience form to build structured output from a mixture of literals and bound match
variables:

```clojure
(mlet ['(?x ?y ?z) '(cat dog bat)]
    (mout '(a ?x (a ?y) and a ?z)))
; → (a cat (a dog) and a bat)
```

`mlet` forms return `nil` if matches fail:

```clojure
(mlet ['(?x ?y ?z) '(cat dog bat frog)]
    (mout '(a ?x a ?y and a ?z)))
; → nil
```

In addition to single element match directives (prefixed with `?`) the matcher supports multiple match directives which
match against zero or more elements of data (these are prefixed with `??`). Multiple directives may also be used in
`matcher-out` expressions, in which case their value is appended into the resulting structure:

```clojure
(mlet ['(??pre x ??post)
       '(mango melon x apple pear berry)]
  (mout '(pre= ?pre post= ??post)))
; → (pre= (mango melon)
;    post= apple pear berry)
```

All patterns may be structured, containing sequences and subsequences (and maps within sequences within maps within
sequences, etc), so it is possible to use patterns to extract data from nested data structures.

### Switching and Specialisation

`mcond` is the most general of the switching/specialisation forms, it can be used to specify a series of pattern based
rules as follows:

```clojure
(mcond [exp]
    ((?x plus ?y)  (+ (? x) (? y)))
    ((?x minus ?y) (- (? x) (? y)))
    )
```

The `mcond` form will attempt to match the data it is given (the value of `exp` in the example above) to the first 
pattern in its sequence of rules `(?x plus ?y)` then its second `(?x minus ?y)` until it finds a rule which matches, it 
then evaluates the body of that rule and returns the result. As with other matcher forms, `mcond` returns `nil` if it 
fails to find a match.  The mcond form above will return `9` if exp has a value of `(5 plus 4)` or `1` if `exp` has a 
value of `(5 minus 4)`. Note that `mcond` (and other forms) can optionally use additional symbols to make their 
rule-based structure more explicit, we recommend using `:=>` for example:

```clojure
(mcond [exp]
    ((?x plus ?y)  :=> (+ (? x) (? y)))
    ((?x minus ?y) :=> (- (? x) (? y)))
    )
```

`defmatch` is similar in structure to `mcond`, wrapping an implicit `mcond` form with a function definition:

```clojure
(defmatch math1 []
  ((?x plus ?y)  :=> (+ (? x) (? y)))
  ((?x minus ?y) :=> (- (? x) (? y)))
  )

(math1 '(4 plus 5))  ; → 9
(math1 '(4 minus 5)) ; → -1
(math1 '(4 times 5)) ; → nil
```

`defmatch` forms can take explicit arguments in addition to their implicit matched-data argument. The example below 
illustrates this and additionally uses an anonymous match variable to handle default cases:

```clojure
(defmatch math2 [x]
  ((add ?y)  :=> (+ x (? y)))
  ((subt ?y) :=> (- x (? y)))
  ( ?_       :=> x)
  )

(math2 '(add 7) 12)   ; → 19
(math2 '(subt 7) 12)  ; → 5
(math2 '(times 7) 12) ; → 12
```

Due to the way patterns may be specified at the symbol level, defmatch forms can be used to specialise on keywords and 
thereby resemble some kind of dispatch, e.g.:

```clojure
(defmatch calcd [x y]
  (:add  :=> (+ x y))
  (:subt :=> (- x y))
  (:mult :=> (* x y))
  )

(calcd :add 5 4)  ; → 9
(calcd :mult 5 4) ; → 20
```

### Searching and Selection

The searching and selection forms match patterns across collections of data, returning the first match which is found. 
These matcher forms are called `mfind` (which matches one pattern across a collection of data) and `mfind*` (which 
consistently matches a group of patterns across a collection of data). The next two examples use the following data:

```clojure
(def food
 '([isa cherry  fruit]   [isa cabbage veg]
   [isa chilli  veg]     [isa apple   fruit]
   [isa radish veg]      [isa leek    veg]
   [color leek  green]   [color chilli  red]
   [color apple green]   [color cherry  red]
   [color cabbage green] [color radish red]
  ))
```

Note that in this example we use vectors in our data, this is perhaps idiomatic but we sometimes prefer wrapping tuples 
as vectors (rather than as lists) and the matcher deals with either vectors or lists (or maps). `mfind` takes one 
pattern:

```clojure
(mfind ['[isa ?f veg] food] (? f))
; → cabbage
```

`mfind*` takes multiple patterns:

```clojure
(mfind* ['([isa ?f veg] [color ?f red])
         food]
    (? f))
; → chilli
```

### Iteration and Collection

The matcher supports two forms to provide iteration and collection capability, these are called `mfor` and `mfor*`. They 
iterate over sets of data using one pattern (`mfor`) or multiple patterns (`mfor*`). The following examples use the food 
data presented above:

```clojure
(mfor ['[isa ?f veg] food] (? f))
; → (cabbage chilli radish leek)

(mfor* ['([isa ?f veg] [color ?f red])
        food]
    (? f))
; → (chilli radish)
```

## General Principles 

### Patterns

Patterns can contain symbols and matcher variables and nested structures made up from lists, vectors and maps. Nesting 
can be to any depth.

### Matcher Variables

Within patterns, matcher variables are prefixed by single or double question marks (`?` or `??`). Match variables 
prefixed by single element match directives (`?`) match with one element of data, multiple match directives match 
against zero or more elements of data (these are prefixed with `??`).

In the following example match variable `a` matches with `'cat` and `b` matches with `'(dog bat)`:

```clojure
(matches '(?a ??b frog) '(cat dog bat frog))
; → {b (dog bat), a cat, :pat (?a ??b frog), :it (cat dog bat frog)}
```

Anonymous match variables (which do not retain their values) are also provided, these are `?_` and `??_`.

```clojure
(matches '(?a ??_ frog ?_) '(cat dog bat frog fish))
→ {tmp230 (dog bat), a cat, :pat (?a ??_ frog ?_), :it (cat dog bat frog fish)}
```

Single and multiple directives may also be used in `matcher-out` expressions. Single match directives have their values 
pushed into the output using cons multiple directives have their value appended into the resulting structure.

```clojure
(mlet ['(??pre bat ??post) '(cat dog bat frog fish)]
  (mout '(pre= ?pre post= ??post)))
; → (pre= (cat dog) post= frog fish)
```

Anonymous match variables do have any meaning in output patterns so are treated as literals:

```clojure
(mlet ['(??pre bat ??_) '(cat dog bat frog fish)]
    (mout '(pre= ?pre post= ??_)))
; → (pre= (cat dog) post= ??_)
```

Moreover, all match variables which are not bound are treated as literals in match output as are any match variables 
which bind to other match variables:

```clojure
(mlet ['(??pre bat ??post) '(cat dog bat ?x ?y)]
   (mout '(pre= ?pre post= ??post)))
; → (pre= (cat dog) post= ?x ?y)
```

This is an important and useful feature - see some of the worked examples for ideas.

Other examples:

```clojure
(mlet ['((a b) {n ?x m ?y}) '((a b) {n 1 m 2})]
    (mout '(n= ?x m= ?y)))
; → (n= 1 m= 2)
```

Note there is a restriction in matching within maps - you can only use match variables to match values, not keys. So 
this works okay:

```clojure
(mlet ['{a ?x, b ?y, c ?z} '{a 1, b 2, c 3}]
    (mout '(?x ?y ?z)))
; → (1 2 3)
```

However, this does not:

```clojure
(mlet ['{a ?x, ?y 2, c ?z} '{a 1, b 2, c 3}]
    (mout '(?x ?y ?z)))
; → nil
```

If you need to search for key values you should specify the match using vectors as follows:

```clojure
(mlet ['[??_ [?x 2] ??_] '{a 1, b 2, c 3}]
    (? x))
; → b
```

See also predicates in patterns.

mlet
mlet (matcher-let) is the most primitive form of matcher expression provided for general use is, it is structured as follows:

(mlet [ pattern data ]
  ...body...
  )

mlet operates as follows: if the pattern matches the datum, binding (zero or more) matcher variables as part of the matching process then mlet evaluates its body in the context of these bindings. If the pattern and datum do not match, mlet returns nil.

In the following example, the pattern (?x ?y ?z) matches the datum (cat dog bat) binding match variables "x", "y", "z" to 'cat, 'dog, 'bat respectively and the expression (? y) in the body of mlet retrieves the value of the match variable "y" from (pseudo) matcher name space.

(mlet ['(?x ?y ?z) '(cat dog bat)]
  (? y))
→ dog

mlet forms return nil if matches fail:

(mlet ['(?x ?y ?z) '(cat dog bat frog)]
    (mout '(a ?x a ?y and a ?z)))
→ nil

Once bound a match variable cannot be implicitly re-bound so whilst the pattern (?x dog ?x) matches (cat dog cat) it will not match (cat dog bat) because this would result in an inconsistent/ambiguous binding for "x". This approach also holds true with nested matcher forms, so given the data (dog bat) the following expression will return (cat dog bat) but with data (rat bat) it will return 'inner-match-failed:

(defn foo [data]
  (mlet ['(?x ?y) '(cat dog)]
    (or (mlet ['(?y ?z) data]
          (mout '(?x ?y ?z)))
      'inner-match-failed)
    ))

(foo '(dog bat)) → (cat dog bat)
(foo '(rat bat)) → inner-match-failed


Unbound matcher variables have nil values as does the anonymous match variable "?_" which will always match with a piece of data  but does not retain the data it matches against:

(mlet ['(?_ ?x) '(cat dog)]
    (list (? _) (? x) (? y)))
→ (nil dog nil)





?
The expression (? y) retrieves the value of the match variable "y" from (pseudo) matcher name space...

(mlet ['(?x ?y ?z) '(cat dog bat)]
  (? y))
→ dog

Unbound matcher variables have nil values as does the anonymous match variable "?_" which will always match with a piece of data  but does not retain the data it matches against:

(mlet ['(?_ ?x) '(cat dog)]
    (list (? _) (? x) (? y)))
→ (nil dog nil)





mout
mout (matcher-out) is a convenience form to build structured output from a mixture of literals and bound match variables:

(mlet ['(?x ?y ?z) '(cat dog bat)]
    (mout '(a ?x (a ?y) and a ?z)))
→ (a cat (a dog) and a bat)

mout preserves the structures of enclosed maps, vectors, lists, etc:
(mlet ['(?a ?b ?c) '(aa bb cc)]
  (mout '(list ?a [vector ?b and map {:a ?a :b ?b ?c ccc}])))
→ (list aa [vector bb and map {:a aa, :b bb, cc ccc}])
"??" directives may also be used in matcher-out expressions, in which case their value is appended into the resulting structure:

(mlet ['(??pre x ??post)
       '(mango melon x apple pear berry)]
  (mout '(pre= ?pre post= ??post)))

→ (pre= (mango melon)
   post= apple pear berry)

mout also allows :eval directives to evaluate Clojure code at mout expansion time. This is usually used to manipulate matcher variable values (see example below). Note that you cannot directly refer to let variables in :eval forms.
(mlet ['(?a ?b ?c) '(1 2 3)]
  (mout '(?a ?b (:eval (+ 1 (? c))))))
→ (1 2 4)




mif
mif (matcher-if) is a matcher equivalent of if...

(mif ['(?a ?b) '(1 2)]   (list 'yip (? a)) 'nope) →  (yip 1))
(mif ['(?a ?b) '(1 2 3)] (list 'yip (? a)) 'nope) →  nope)

Like if the else clause of mif is optional...

(mif ['(?a ?b) '(1 2)]   (list 'yip (? a))) → (yip 1))
(mif ['(?a ?b) '(1 2 3)] (list 'yip (? a))) → nil)






massert
massert offers a run-time assertion mechanism based on patterns

user=> (massert ['(?a ?b) '(cat dog)] "whoops no match")
nil

user=> (massert ['(?a ?b) '(cat dog bat)] "whoops no match")
RuntimeException whoops no match  user/eval276 (NO_SOURCE_FILE:80)

Note: the precise output of massert when it fails will depend on its run-time context.





mcond
mcond is the most general of the switching/specialisation forms, it can be used to specify a series of pattern based rules as follows:

(mcond [exp]
    ((?x plus ?y)  (+ (? x) (? y)))
    ((?x minus ?y) (- (? x) (? y)))
    )

The mcond form will attempt to match the data it is given (the value of exp in the example above) to the first pattern in its sequence of rules (?x plus ?y) then its second (?x minus ?y) until it finds a rule which matches, it then evaluates the body of that rule and returns the result. As with other matcher forms, mcond returns nil if it fails to find a match.  The mcond form above will return 9 if exp has a value of (5 plus 4) or 1 if exp has a value of (5 minus 4).




:=>
mcond, defmatch (and other forms) can optionally use additional symbols to make their rule-based structure more explicit, we recommend using ":=>" for example:

(mcond [exp]
    ((?x plus ?y)  :=> (+ (? x) (? y)))
    ((?x minus ?y) :=> (- (? x) (? y)))
    )





defmatch
defmatch is similar in structure to mcond, wrapping an implicit mcond form with a function definition:

(defmatch math1 []
  ((?x plus ?y)  :=> (+ (? x) (? y)))
  ((?x minus ?y) :=> (- (? x) (? y)))
  )

(math1 '(4 plus 5))  → 9
(math1 '(4 minus 5)) → -1
(math1 '(4 times 5)) → nil

defmatch forms can take explicit arguments in addition to their implicit matched-data argument. The example below illustrates this and additionally uses an anonymous match variable to handle default cases:

(defmatch math2 [x]
  ((add ?y)  :=> (+ x (? y)))
  ((subt ?y) :=> (- x (? y)))
  ( ?_       :=> x)
  )

(math2 '(add 7) 12) → 19
(math2 '(subt 7) 12) → 5
(math2 '(times 7) 12) → 12

Due to the way petterns may be specified at the symbol level, defmatch forms can be used to specialise on keywords and thereby resemble some kind of dispatch, eg:

(defmatch calcd [x y]
  (:add  :=> (+ x y))
  (:subt :=> (- x y))
  (:mult :=> (* x y))
  )

(calcd :add 5 4)  → 9
(calcd :mult 5 4) → 20






mfn
This is the matcher equivalent of fn, which builds an anonymous defmatch form.

(map (mfn []
        ((:add ?x ?y)  :=> (+ (? x) (? y)))
        ((:subt ?x ?y) :=> (- (? x) (? y))))
  '((:add 4 5)(:add 9 3)(:subt 9 3)))
→ (9 12 6)






mvars & with-mvars
A pseudo matcher name space is maintained, this is not a Clojure name space but rather it is a map (called mvars) which associates named matcher variables with their values. mvars is a lexically bound Clojure symbol accessible within the body of all matcher expressions.

with-mvars provides a simple way to inject variables into matcher name space or shadow existing values. See the following example:

(with-mvars {'a (+ 2 3), 'b (- 3 4)}
  (println mvars)
  (with-mvars {'b 'bb, 'd 'xx, 'e 'yy}
    (println "  " mvars)
    (mlet ['(?a ?b ?d ?c) '(5 bb xx spam)]
      (println "    " mvars))
    (println "  " mvars))
  (println mvars))

output...
{b -1, a 5}
   {e yy, d xx, b bb, a 5}
     {c spam, :pat (?a ?b ?d ?c),
        :it (5 bb xx spam),
        e yy, d xx, b bb, a 5}
   {e yy, d xx, b bb, a 5}
{b -1, a 5}
nil





:it
Note that the matcher adds the last datum that was match (called :it) and the last pattern :it was matched against into name space.

While direct reference to mvars is generally unnecessary, it is useful for writing new macros and it allows the results of successful matching operations to be saved for later processing or passed to other functions in cases where the lexical scoping of matcher variables is found restrictive.






mfind & mfind*
The searching and selection forms, mfind & mfind*, match patterns across collections of data, returning the first match which is found. mfind matches one pattern across a collection of data and mfind* consistently matches a group of patterns across a collection of data. The next two examples use the following data:

(def food
 '([isa cherry  fruit]   [isa cabbage veg]
   [isa chilli  veg]     [isa apple   fruit]
   [isa radish veg]      [isa leek    veg]
   [color leek  green]   [color chilli  red]
   [color apple green]   [color cherry  red]
   [color cabbage green] [color radish red]
  ))

Note that in this example we use vectors in our data, this is perhaps idiomatic but we sometimes prefer wrapping tuples as vectors (rather than as lists) and the matcher deals with either vectors or lists (or maps).

mfind takes one pattern:

(mfind ['[isa ?f veg] food] (? f))
→ cabbage

mfind* takes multiple patterns:

(mfind* ['([isa ?f veg] [color ?f red])
         food]
    (? f))
→ chilli






mfor & mfor*
The matcher supports two forms to provide iteration and collection capability, these are called mfor and mfor*. They iterate over sets of data using one pattern (mfor) or multiple patterns (mfor*). The following examples use the following food data:

(def food
 '([isa cherry  fruit]   [isa cabbage veg]
   [isa chilli  veg]     [isa apple   fruit]
   [isa radish veg]      [isa leek    veg]
   [color leek  green]   [color chilli  red]
   [color apple green]   [color cherry  red]
   [color cabbage green] [color radish red]
  ))


mfor takes one pattern:

(mfor ['[isa ?f veg] food] (? f))
→ (cabbage chilli radish leek)


mfor* takes multiple patterns:

(mfor* ['([isa ?f veg] [color ?f red])
        food]
    (? f))
→ (chilli radish)






predicates in patterns
Patterns may associate predicates with match variables inside the patterns which use them. This allows potential match bindings to be validated and causes matches to fail if the predicates are not satisfied. The syntax for specifying a predicate is...
(-> ?match-var predicates)


The following pattern will only match ?x against a number...

(mlet ['(??pre (-> ?x number?) ??post) '(a b 5 c d e)]
  (mout '(x= ?x  pre= ?pre  post= ?post)))
→ (x= 5 pre= (a b) post= (c d e))

When predicates fail so does the matching...

(mlet ['(??pre (-> ?x number?) ??post) '(a b c d e)]
  (mout '(x= ?x  pre= ?pre  post= ?post)))
→ nil

You can use multiple predicates as follows...

(mlet ['(??pre (-> ?x number? odd?) ??post) '(a b 5 c d e)]
  (mout '(x= ?x  pre= ?pre  post= ?post)))
→ (x= 5 pre= (a b) post= (c d e))

(mlet ['(??pre (-> ?x number? even?) ??post) '(a b 5 c d e)]
  (mout '(x= ?x  pre= ?pre  post= ?post)))
→ nil

Note that predicates allow values to be changed as they are matched. Predicates work as follows:
if a match variable m matches data d using predicate p, eg:

(matches '(... (-> ?m p) ...) '(... d ...))

then...
if p(d) = true, {m → d}   ie: the match succeeds & m is bound to d
if p(d) = false|nil  the match fails

but...
if p(d) = d2, {m → d2} where d2 is some new value which is not true, false or nil then the match variable m is bound to d2 not to d

eg:

(inc 12) → 13

(mlet ['(start (-> ?x inc) end) '(start 5 end)]
  (? x))
→ 6

Use can also use this approach to bind many values using (-> ??var predicate), the next example combines a few ideas, using an inline function definition as a predicate...

(mlet ['(start (-> ??x #(= (count %) 3)) ??rest end) '(start a b c d e end)]
  (? x))
→ (a b c)

This facility allows the matcher to be used as a reconstructive parser - a parser which returns some constructed result on successful parsing (an example of this is provided in the examples section).






:not and :guard

:not and :guard may only be used with pattern collections used by find* and for*. They allow logical checks to be made in pattern collections. The following example uses :not to match a one-way relation only...

(mfind* ['((?a ?b) (:not (?b ?a)))
         '((p q)(x y)(q r)(y x))
        ]
  (mout '(?a ?b)))
→ (p q)

The next example uses a guard to ensure matching with even numbers only...

(mfind* ['((?a ?b) (:guard (even? (? b))))
         '((a 1) (b 2) (c 3) (d 4))
        ]
  (mout '(?a ?b)))
→ (b 2)

...note that matcher variables are scoped within :guard forms.

You may use multiple forms with both :guard and :not in which case they are logically and'ed in sequence...

(mfind* ['((?a ?b) (:guard (number? (? b)) (even? (? b))))
         '((a 1)(x y)(b 2)(y z)(c 3)(d 4))
        ]
  (mout '(?a ?b)))
→ (b 2)


a fuller example
This provides an alternative to the example used in the section on state-changing operators. We recommend you read that first.

This approach uses predicates, :not and :guard to provide an alternative set of state changing operators.

We consider this kind of stste definition...

(def state1
  '#{(at Sue table)
     (at book table)
     (place table)
     (place bench)
     })

For convenience we use some definitions...

(def agents '#{Sue Sam})
(def manipulables '#{book cup})

(defn agent? [x] (agents x))
(defn manipulable? [x] (manipulables x))

Now we define operators using predicates, :not & :guard...

(def ops
  '{pickup {:pre ( (at (-> ?agent agent?)      ?place)
                   (at (-> ?obj  manipulable?) ?place)
                   (:not (holds ?agent ?_))
                   )
            :add ((holds ?agent ?obj))
            :del ((on ?obj   ?place))
            }
    drop    {:pre ((at ?agent ?place)
                    (holds ?agent ?obj)
                    )
             :add ((at ?obj   ?place))
             :del ((holds ?agent ?obj))
             }
    move    {:pre ( (at (-> ?agent agent?) ?place)
                    (place ?p2)
                    (:guard (not= (? p1) (? p2)))
                    )
             :add ((at ?agent ?p2))
             :del ((at ?agent ?p1))
             }
    })


apply-op is the same as in the other state-changing examples...

(defn apply-op
  [state {:keys [pre add del]}]
  (mfind* [pre state]
    (union (mout add)
      (difference state (mout del))
      )))


Now we can use the operators...

(apply-op state1 (ops 'pickup))
→  #{(holds Sue book) (place table) (at Sue table) (place bench) (at book table)}


(-> state1
         (apply-op ('pickup ops))
         (apply-op ('move   ops))
         (apply-op ('drop   ops)))
→  #{(place table) (at Sue table) (place bench) (at book table)}






example: searching sets of tuples

This example considers searching for objects in a set of tuples which describe the state of a micro-world. To put this in context: we receive object descriptions (and other forms) from language processing subsystem so, for example, the noun-phrase "red fruit" would produce...

(obj
  (quantifier all)
  (desc ((color red) (isa fruit))))

...and the phrase "a large red fruit" would produce...

(obj
  (quantifier any)
  (desc
    ((size large) (color red) (isa fruit))))

We store state information in the following form...

(def food
  '#{[isa chilli veg]    [isa cherry fruit]
     [isa radish veg]    [isa apple fruit]
     [isa leek veg]      [isa kiwi fruit]
     [color chilli red]  [color cherry red]
     [color radish red]  [color apple green]
     [color leek green]  [color kiwi green]
     [on chilli table]   [on cherry table]
     [on leek table]
     })

Our aim is to write code which, using the type of object descriptions from the language processing subsystem, can retrieve the relevant object names. We can use mfor to find the names of objects for a single type of fact/tuple. For example, the following form returns the names of all cubes:

(mfor ['(isa ?obj veg) food]
    (? obj))
→ (chilli leek radish)

Wrapping this mfor expressing in a match function provides a means to obtain object names for the types of (relation value) pairs provided by the language processing subsystem:

(defmatch find-all [tuples]
  ([?reln ?val]
    (mfor ['(?reln ?obj ?val) tuples]
      (? obj)
      )))

(find-all '(isa veg) food)
→ (chilli leek radish)

If the results of multiple find-all expressions are converted to sets multiple (relation value) pairs can be handled using set intersection. So to find red vegetable from the food data:

(find-all '(isa veg) food)
→ (chilli leek radish)

(find-all '(color red) food)
→ (chilli radish cherry)

(intersection
    (set '(chilli leek radish))
    (set '(chilli radish cherry)))
→ #{radish chilli}

This processing can be captured in a function as follows:

(defn query
  [reduction pairs tuples]
  (reduce reduction
    (map #(set (find-all % tuples)) pairs))
  )

(query intersection
    '((isa veg)(color red)) food)
→ #{radish chilli}

The query function may also be used with union to return "or" combinations:

(query union
    '((isa veg)(color red)) food)
→ #{cherry radish chilli leek}

To satisfy our initial aim we therefore need the following...

(defmatch find-all [tuples]
  ([?reln ?val]
    (mfor ['(?reln ?obj ?val) tuples]
      (? obj)
      )))

(defn query
  [reduction pairs tuples]
  (reduce reduction
    (map #(set (find-all % tuples)) pairs))
  )






example: applying rules

This example considers a rule-based, fact deduction or forward chaining mechanism. Facts are held as tuples and rules have antecedents and consequents. Some introductory texts for Artificial Intelligence provide example rules like...

IF (has fido hair) THEN (isa fido mammal)

...while these serve to illustrate their discussion of rule-based inference, rules like this are of limited use because they are specific to object names ("fido" in this case) and take only a single antecedent and consequent. For practical purposes rules need to be flexible about the length of their antecedents/consequents and allow both to include variables. For our work we wish to use rules like the following...

(rule 15 (parent ?a ?b) (parent ?b ?c)
      => (grandparent ?a ?c))

...which would work on data like...

(def family
  '( (parent Sarah Tom)
     (parent Steve Joe)
     (parent Sally Sam)
     (parent Ellen Sarah)
     (parent Emma  Bill)
     (parent Rob   Sally)))

A suitable rule application mechanism needs to split the rule into its constituent parts; search for all consistent sets of antecedents; ripple any antecedent variable bindings through to consequents and collect evaluated consequents for each rule every time it fires. In practice these requirements can be by using a match function to pull a rule apart, mfor* to satisfy all possible antecedent combinations and mout to bind variables into consequents. This can be specified as follows...

(defmatch apply-rule [facts]
  ((rule ?n ??antecedents => ??consequents)
    :=> (mfor* [(? antecedents) facts]
          (mout (? consequents))))
  )

(apply-rule
  '(rule 15 (parent ?a ?b) (parent ?b ?c)
             => (grandparent ?a ?c))
  family)

→ ((grandparent Ellen Tom)
   (grandparent Rob Sam))

Notice that while the pattern for defmatch is literally specified, the patterns for mfor* and mout must, necessarily, be generated dynamically. Furthermore these dynamically generated patterns are embedded in the rule structure pulled apart by defmatch's literal pattern.

To investigate this rule deduction example further we use a richer set of facts and rules (where the consequences of some rules trigger the antecedents of others)...

(def facts1
  '((big elephant)  (small mouse)
    (small sparrow) (big whale)
    (on elephant mouse)
    (on elephant sparrow)
    ))

(def rules1
   '((rule 0 (heavy ?x)(small ?y)(on ?x ?y)
             => (squashed ?y) (sad ?x))
     (rule 1 (big ?x)   => (heavy ?x))
     (rule 2 (light ?x) => (portable ?x))
     (rule 3 (small ?x) => (light ?x))
     ))

Given these definitions it is possible to develop a function to apply all rules once...

(defn apply-all [rules facts]
  (reduce concat
    (map #(apply-rule % facts) rules)
    ))

(apply-all rules1 facts1)
→ ((heavy elephant) (heavy whale)
   (light mouse) (light sparrow))

For simplicity in combining the output of rules we use sets so modify the apply-all function a little...

(defn apply-all [rules facts]
  (set (reduce concat
         (map #(apply-rule % facts) rules)
         )))

...then develop a forward chaining/fact deduction function which continues to operate while it is generating new facts...

(defn fwd-chain [rules facts]
  (let [new-facts (apply-all rules facts)]
    (if (subset? new-facts facts)
      facts
      (recur rules (union facts new-facts))
      )))

(fwd-chain rules1 (set facts1))
→  #{(light mouse) (heavy elephant)
     (on elephant sparrow)
     (squashed sparrow)
     (small sparrow) (on elephant mouse)
     (squashed mouse) (small mouse)
     (portable sparrow) (big whale)
     (portable mouse) (big elephant)
     (sad elephant) (light sparrow)
     (heavy whale)}

As with the other examples, the matcher performs most of the processing (in this case using a defmatch construct and mfor* in apply-rule) while other functions collate results, etc.

(defmatch apply-rule [facts]
  ((rule ?n ??antecedents => ??consequents)
    :=> (mfor* [(? antecedents) facts]
          (mout (? consequents))))
  )

(defn apply-all [rules facts]
  (reduce concat
    (map #(apply-rule % facts) rules)
    ))

(defn fwd-chain [rules facts]
  (let [new-facts (apply-all rules facts)]
    (if (subset? new-facts facts)
      facts
      (recur rules (union facts new-facts))
      )))






example: applying state changing operators

In this example we consider how to apply the kind of state changing operators that are used in some planning systems. Broadly we adapt a representation borrowed from PDDL for use with a STRIPS style solver. The operators are specified in terms of their preconditions and their effects. We use tuples to capture state information. The following tuples, for example, describe a simple state in which some (animated) agent (R) is at a table is holding nothing and a book is on the table.

#{(at R table)
  (on book table)
  (holds R nil)
  (path table bench)
  (manipulable book)
  (agent R)
  }

In order to generalise an operator (so it can be used with different agents, objects and in various locations) it is necessary to specify it using variables, in this case matcher variables. An operator which describes a "pickup" activity for an agent and which can be used to produces a new state (new tuples) could be described as follows...

  {:pre ((agent ?agent)
         (manipulable ?obj)
         (at ?agent ?place)
         (on ?obj   ?place)
         (holds ?agent nil)
         )
   :add ((holds ?agent ?obj))
   :del ((on ?obj   ?place)
         (holds ?agent nil))
   }

The operator is map with three components (i) a set of preconditions which must be satisfied in order for the operator to be used (ii) a set of tuples to add to an existing state when producing a new state and (iii) a set of tuples to delete from an existing state.

To apply this kind of operator specification we extract patterns from the operator then use mfind*

(defn apply-op
  [state {:keys [pre add del]}]
  (mfind* [pre state]
    (union (mout add)
      (difference state (mout del))
      )))

 (apply-op state1 ('pickup ops))
 →  #{(agent R) (holds R book)
      (manipulable book)
      (path table bench) (at R table)}

The patterns used by mfind* are provided dynamically when apply-op is called and furthermore the patterns themselves define the semantics of the operators.

Collections of operators are conveniently held in a map, and ordered sequences of operator applications can be formed by chaining apply-op calls, eg:

(def ops
  '{pickup {:pre ((agent ?agent)
                  (manipulable ?obj)
                  (at ?agent ?place)
                  (on ?obj   ?place)
                  (holds ?agent nil)
                  )
            :add ((holds ?agent ?obj))
            :del ((on ?obj   ?place)
                  (holds ?agent nil))
            }
    drop    {:pre ((at ?agent ?place)
                   (holds ?agent ?obj)
                   )
             :add ((holds ?agent nil)
                   (on ?obj   ?place))
             :del ((holds ?agent ?obj))
             }
    move    {:pre ((agent ?agent)
                   (at ?agent ?p1)
                   (path ?p1 ?p2)
                   )
             :add ((at ?agent ?p2))
             :del ((at ?agent ?p1))
             }
    })

(-> state1
  (apply-op ('pickup ops))
  (apply-op ('move   ops))
  (apply-op ('drop   ops)))

→ #{(agent R) (manipulable book)
    (on book bench) (holds R nil)
    (at R bench) (path table bench)}






writing dispatchers

Due to the way patterns may be specified at the symbol level, defmatch forms can be used to specialise on keywords and thereby resemble some kind of dispatch, e.g.

(defmatch calcd [x y]
  (:add  :=> (+ x y))
  (:subt :=> (- x y))
  (:mult :=> (* x y))
  )

(calcd :add 5 4)  →  9
(calcd :mult 5 4) →  20

By using predicates defmatch forms can also specialise on types and thereby dispatch on types (see the notes on predicates for more details)...

(defmatch dispatch []
  ((-> ?x number?) :=> (* 2 (? x)))
  ((-> ?x seq?)    :=> (first (? x)))
  ( ?x             :=> (? x))
  )

(dispatch 7)  →  14
(dispatch '(a b c))  →  a
(dispatch 'banana)   →  banana


Predicates can also modify bound values, check the next 2 examples. The first is a standard dispatch on types, the second uses in-pattern value modification...

(defmatch sizer []
  ((-> ?x map?)    :=> (-> (? x) keys count))
  ((-> ?x coll?)   :=> (count (? x)))
  ((-> ?x string?) :=> (count (? x)))
  ((-> ?x symbol?) :=> 1)
  )

(sizer {:a 1 :b 2 :c 3})   →  3
(sizer "banana")  →  6
(sizer 'banana)   →  1


Example using in-pattern value modification...

(defmatch sizer []
  ((-> ?x map? keys count) :=> (? x))
  ((-> ?x coll?   count)   :=> (? x))
  ((-> ?x string? count)   :=> (? x))
  ((-> ?x symbol?) :=> 1)
  )

(sizer {:a 1 :b 2 :c 3})  →  3
(sizer "banana")    →  6
(sizer '(a b c d))  →  4


For our last example we use a function (could we almost start to call it a method?) which specialises on more than one of its argument types...

(defn add [& args]
  (mcond [args]
    (((-> ?a number?) (-> ?b number?)) :=> (+ (? a) (? b)))
    (((-> ?n number?) (-> ?s seq?))    :=> (map #(+ % (? n)) (? s)))
    (((-> ?a seq?)    (-> ?b seq?))    :=> (concat (? a) (? b)))
  ))

(add '(a b c) '(d e f))  →  (a b c d e f)
(add 5 17)        →  22
(add 5 '(1 3 9))  →  (6 8 14)







specifying a grammar with the matcher

Grammars can be used to generate "sentences" or to produce phrase-structures (parse trees) from strings of words. This example investigates the latter, using the matcher to define the grammar rules.

the problem...
given a sentence like "a cat chased the large rat" we want to produce a phrase structure tree something like...

(sentence '(a cat chased the large rat))
→ (sentence (noun-phrase (det a) (noun cat))
            (verb-phrase (verb chased)
                         (noun-phrase
                            (det the)
                            (adj large)
                            (noun rat))))

simple categories
We can deal with individual words fairly easily, for example if "cat" is a noun then parsing the word "cat" on its own should return...

(cat noun)

One way of doing this is to have a set of all nouns & use a category checking function...


(def nouns '#{cat bat rat kipper melon})

(defn check-category [cat-name word word-set]
  (and (word-set word)
    (list cat-name word)
    ))

(defn noun [x] (check-category 'noun x nouns))

(noun 'cat)
→ (noun cat)

(noun 'thirteen)    ;; returns nil if it fails
→ nil
We can do something similar for other categories of word...

;; word definitions
(def nouns '#{cat bat rat kipper melon})
(def verbs '#{ate chased})
(def dets  '#{the a every})
(def adjs  '#{large small})

;; word checks
(defn noun [x] (check-category 'noun x nouns))
(defn verb [x] (check-category 'verb x verbs))
(defn det [x] (check-category 'det x dets))
(defn adj [x] (check-category 'adj x adjs))

(noun 'thirteen) → nil

(det 'every) → (det every)
(adj 'every) → nil
(adj 'small) → (adj small)

non-terminal categories
We need a different style of solution for non-terminal categories like noun-phrases (which are made up of multiple words). The matcher allows predicates to be used to enforce restrictions on match patterns...

(mlet ['(??a ?x ??b) '(a b c 10 d e f)]
    (mout '((a ?a)(b ?b)(x ?x))))
→ ((a ()) (b (b c 10 d e f)) (x a))

(mlet ['(??a (-> ?x number?) ??b) '(a b c 10 d e f)]
    (mout '((a ?a)(b ?b)(x ?x))))
→ ((a (a b c)) (b (d e f)) (x 10))

If we use some variation of a predicate which returns something other than true to indicate success, the matcher binds this non-nil value to the relevant match variable. For example...

(defn nsquare? [x]
  (and (number? x)
    (* x x)))

(nsquare? 'banana) → nil

(nsquare? 10) → 100

(mlet ['(??a (-> ?x nsquare?) ??b) '(a b c 10 d e f)]
    (mout '((a ?a)(b ?b)(x ?x))))
→ ((a (a b c)) (b (d e f)) (x 100))

We can use this to build parse expressions for our non-terminal categories.

(mlet ['((-> ?d det) (-> ?n noun)) '(the cat)]
   (mout '(noun-phrase ?d ?n)))
→ (noun-phrase (det the) (noun cat))

(defmatch noun-phrase []
  (((-> ?d det) (-> ?n noun)) :=> (mout '(noun-phrase ?d ?n))))

(noun-phrase '(the cat))
→ (noun-phrase (det the) (noun cat))

(noun-phrase '(kipper melon))
→ nil

There is no problem with defining multiple noun-phrase rules because it is a defmatch form...

(defmatch noun-phrase []
  (((-> ?d det) (-> ?n noun))             :=> (mout '(noun-phrase ?d ?n)))
  (((-> ?d det) (-> ?a adj) (-> ?n noun)) :=> (mout '(noun-phrase ?d ?a ?n)))
  )

(noun-phrase '(a large cat))
→ (noun-phrase (det a) (adj large) (noun cat))

(noun-phrase '(every small kipper))
→ (noun-phrase (det every) (adj small) (noun kipper))

(noun-phrase '(the melon))
→ (noun-phrase (det the) (noun melon))

 Now we define another couple of forms to build on noun-phrase...

;; sentence rule
(defmatch sentence []
  (((-> ??np noun-phrase) (-> ??vp verb-phrase)) :=> (mout '(sentence ?np ?vp))))

;; verb phrase
(defmatch verb-phrase []
  (((-> ?v verb) (-> ??np noun-phrase)) :=> (mout '(verb-phrase ?v ?np))))


(sentence '(the large cat ate a small kipper))
→ (sentence (noun-phrase (det the) (adj large) (noun cat))
            (verb-phrase (verb ate)
                (noun-phrase (det a) (adj small) (noun kipper))))

the finished work...

;; sentence rule
(defmatch sentence []
  (((-> ??np noun-phrase) (-> ??vp verb-phrase)) :=> (mout '(sentence ?np ?vp))))


;; noun-phrase rules
(defmatch noun-phrase []
  (((-> ?d det) (-> ?n noun))             :=> (mout '(noun-phrase ?d ?n)))
  (((-> ?d det) (-> ?a adj) (-> ?n noun)) :=> (mout '(noun-phrase ?d ?a ?n)))
  )


;; verb phrase
(defmatch verb-phrase []
  (((-> ?v verb) (-> ??np noun-phrase)) :=> (mout '(verb-phrase ?v ?np))))


;; word definitions
(def nouns '#{cat bat rat kipper melon})
(def verbs '#{ate chased})
(def dets  '#{the a every})
(def adjs  '#{large small})


;; word checks
(defn noun [x] (check-category 'noun x nouns))
(defn verb [x] (check-category 'verb x verbs))
(defn det [x] (check-category 'det x dets))
(defn adj [x] (check-category 'adj x adjs))


;; category helper
(defn check-category [cat-name word word-set]
  (and (word-set word)
    (list cat-name word)
    ))



