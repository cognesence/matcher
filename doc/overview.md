# Overview
This document provides an overview of the features of the pattern matcher. For 
more details refer to the user guide.

## Contents

* [Installation](#installation)
* [Getting Started](#getting-started)
  + [Switching and Specialisation](#switching-and-specialisation)
  + [Searching and Selection](#searching-and-selection)
  + [Iteration and Collection](#iteration-and-collection)
* [Applying Rules](#applying-rules)
* [Applying State-Changing Operators](#applying-state-changing-operators)

## Installation

This library is hosted on clojars. Get it by adding `org.clojars.cognesence/matcher` to your dependencies in your
Leiningen `project.clj` file.

```
(defproject com.example/myproject "1.0.0"
  :description "My Leiningen project!"
  :url "http://example.com/projects/myproject"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
               [org.clojars.cognesence/matcher "1.0.1"]])

```

## Getting Started

The most primitive form of matcher expression provided for general use is `mlet`
(matcher-let), it is structured as follows:

```clojure
(mlet [ pattern datum ]
  ; body
  )
```

`mlet` operates as follows: if the pattern matches the `datum`, binding any 
matcher variables as part of the matching process then `mlet` evaluates its body 
in the context of these bindings. If the `pattern` and `datum` do not match, 
`mlet` returns `nil`.

Match variables are prefixed with a `?` (or `??` - see later), in the following 
example, the pattern `(?x ?y ?z)` matches the datum `(cat dog bat)` binding match 
variables `x`, `y`, `z` to `'cat`, `'dog`, `'bat` respectively and the expression 
`(? y)` in the body of `mlet` retrieves the value of the match variable `y` from 
matcher namespace.

```clojure
(mlet ['(?x ?y ?z) '(cat dog bat)]
  (? y))
  
; ? dog
```

`mout` (matcher-out) is a convenience form to build structured output from a 
mixture of literals and bound match variables:

```clojure
(mlet ['(?x ?y ?z) '(cat dog bat)]
  (mout '(a ?x (a ?y) and a ?z)))
  
; ? (a cat (a dog) and a bat)
```

`mlet` forms return `nil` if matches fail:

```clojure
(mlet ['(?x ?y ?z) '(cat dog bat frog)]
  (mout '(a ?x a ?y and a ?z)))
  
; ? nil
```

In addition to _single element match directives_ (prefixed with `?`) the matcher 
supports _multiple match directives_ which match against zero or more elements 
of data (these are prefixed with `??`). Multiple directives may also be used in 
matcher-out expressions, in which case their value is appended into the 
resulting structure:

```clojure
(mlet ['(??pre x ??post)
  '(mango melon x apple pear berry)]
  (mout '(pre= ?pre post= ??post)))
  
; ? (pre= (mango melon)
;    post= apple pear berry)
```

All patterns may be structured, containing sequences and subsequences (and maps 
within sequences within maps within sequences, etc), so it is possible to use 
patterns to extract data from nested data structures.

### Switching and Specialisation

`mcond` is the most general of the switching/specialisation forms, it can be 
used to specify a series of pattern based rules as follows:

```clojure
(mcond [exp]
  ((?x plus ?y)  (+ (? x) (? y)))
  ((?x minus ?y) (- (? x) (? y))))
```

The `mcond` form will attempt to match the data it is given (the value of `exp`
in the example above) to the first pattern in its sequence of rules `(?x plus ?y)`
then its second `(?x minus ?y)` until it finds a rule which matches, it then 
evaluates the body of that rule and returns the result. As with other matcher 
forms, `mcond` returns `nil` if it fails to find a match.  The `mcond` form above will 
return `9` if `exp` has a value of `(5 plus 4)` or `1` if `exp` has a value of 
`(5 minus 4)`. Note that `mcond` (and other forms) can optionally use additional 
symbols to make their rule-based structure more explicit, we recommend using 
`:=>` for example:

```clojure
(mcond [exp]
  ((?x plus ?y) :=> (+ (? x) (? y)))
  ((?x minus ?y) :=> (- (? x) (? y))))
```

`defmatch` is similar in structure to `mcond`, wrapping an implicit `mcond` form 
with a function definition:

```clojure
(defmatch math1 []
  ((?x plus ?y)  :=> (+ (? x) (? y)))
  ((?x minus ?y) :=> (- (? x) (? y)))
  )

(math1 '(4 plus 5)) ; ? 9

(math1 '(4 minus 5)) ; ? -1

(math1 '(4 times 5)) ; ? nil
```

`defmatch` forms can take explicit arguments in addition to their implicit 
matched-data argument. The example below illustrates this and additionally uses 
an anonymous match variable to handle default cases:

```clojure
(defmatch math2 [x]
  ((add ?y) :=> (+ x (? y)))
  ((subt ?y) :=> (- x (? y)))
  (?_ :=> x))

(math2 '(add 7) 12) ; ? 19

(math2 '(subt 7) 12) ; ? 5

(math2 '(times 7) 12) ; ? 12
```

Due to the way patterns may be specified at the symbol level, `defmatch` forms can 
be used to specialise on keywords and thereby resemble some kind of dispatch, 
for example:

```clojure
(defmatch calcd [x y]
  (:add  :=> (+ x y))
  (:subt :=> (- x y))
  (:mult :=> (* x y)))

(calcd :add 5 4) ; ? 9

(calcd :mult 5 4) ; ? 20
```

### Searching and Selection

The searching and selection forms match patterns across collections of data, 
returning the first match which is found. These matcher forms are called `mfind`
(which matches one pattern across a collection of data) and `mfind*` (which 
consistently matches a group of patterns across a collection of data). The next 
two examples use the following data:

```clojure
(def food
 '([isa cherry fruit] [isa cabbage veg]
   [isa chilli veg] [isa apple fruit]
   [isa radish veg] [isa leek veg]
   [color leek green] [color chilli red]
   [color apple green] [color cherry red]
   [color cabbage green] [color radish red]))
```

Note that in this example we use vectors in our data, this is perhaps idiomatic 
but we sometimes prefer wrapping tuples as vectors (rather than as lists) and 
the matcher deals with either vectors or lists (or maps).

`mfind` takes one pattern:

```clojure
(mfind ['[isa ?f veg] food] (? f))

; ? cabbage
```

`mfind*` takes multiple patterns:

```clojure
(mfind* ['([isa ?f veg] [color ?f red])
         food]
    (? f))

; ? chilli
```

### Iteration and Collection

The matcher supports two forms to provide iteration and collection capability, these are called `mfor` and `mfor*`. They iterate over sets of data using one pattern (`mfor`) or multiple patterns (`mfor*`). The following examples use the food data presented above:

```clojure
(mfor ['[isa ?f veg] food] (? f))

; ? (cabbage chilli radish leek)

(mfor* ['([isa ?f veg] [color ?f red]) food]
  (? f))

; ? (chilli radish)
```

## Applying Rules

This example considers a rule-based, fact deduction or forward chaining 
mechanism. Facts are held as tuples and rules have antecedents and consequents. 
Some introductory texts for Artificial Intelligence provide example rules 
like:

```
IF (has fido hair) THEN (isa fido mammal)
```

While these serve to illustrate their discussion of rule-based inference, rules
like this are of limited use because they are specific to object names (`fido`
in this case) and take only a single antecedent and consequent. For practical 
purposes rules need to be flexible about the length of their 
antecedents/consequents and allow both to include variables. For our work we 
wish to use rules like the following:

```clojure
(rule 15 (parent ?a ?b) (parent ?b ?c)
      => (grandparent ?a ?c))
```

Which would work on data like:

```clojure
(def family
  '((parent Sarah Tom)
    (parent Steve Joe)
    (parent Sally Sam)
    (parent Ellen Sarah)
    (parent Emma Bill)
    (parent Rob Sally)))
```

A suitable rule application mechanism needs to split the rule into its constituent parts; search for all consistent sets of antecedents; ripple any antecedent variable bindings through to consequents and collect evaluated consequents for each rule every time it fires. In practice these requirements can be by using a match function to pull a rule apart, `mfor*` to satisfy all possible antecedent combinations and mout to bind variables into consequents. This can be specified as follows...

```clojure
(defmatch apply-rule [facts]
  ((rule ?n ??antecedents => ??consequents)
    :=> (mfor* [(? antecedents) facts]
          (mout (? consequents))))
  )

(apply-rule
  '(rule 15 (parent ?a ?b) (parent ?b ?c)
            => (grandparent ?a ?c))
  family)

; ? ((grandparent Ellen Tom)
;    (grandparent Rob Sam))
```

Notice that while the pattern for `defmatch` is literally specified, the patterns 
for `mfor*` and `mout` must, necessarily, be generated dynamically. Furthermore 
these dynamically generated patterns are embedded in the rule structure pulled 
apart by the literal pattern in `defmatch`.

To investigate this rule deduction example further we use a richer set of facts 
and rules (where the consequences of some rules trigger the antecedents of 
others):

```clojure
(def facts1
  '((big elephant) (small mouse)
    (small sparrow) (big whale)
    (on elephant mouse)
    (on elephant sparrow)))

(def rules1
   '((rule 0 (heavy ?x)(small ?y)(on ?x ?y)
             => (squashed ?y) (sad ?x))
     (rule 1 (big ?x)   => (heavy ?x))
     (rule 2 (light ?x) => (portable ?x))
     (rule 3 (small ?x) => (light ?x))
     ))
```

Given these definitions it is possible to develop a function to apply all rules 
once:

```clojure
(defn apply-all [rules facts]
  (reduce concat
    (map #(apply-rule % facts) rules)
    ))

(apply-all rules1 facts1)

; ? ((heavy elephant) (heavy whale)
;    (light mouse) (light sparrow))
```

For simplicity in combining the output of rules we use sets so modify the 
`apply-all` function a little:

```clojure
(defn apply-all [rules facts]
  (set (reduce concat
         (map #(apply-rule % facts) rules)
         )))
```

Then develop a forward chaining/fact deduction function which continues to 
operate while it is generating new facts:

```clojure
(defn fwd-chain [rules facts]
  (let [new-facts (apply-all rules facts)]
    (if (subset? new-facts facts)
      facts
      (recur rules (union facts new-facts))
      )))

(fwd-chain rules1 (set facts1))

; ?  #{(light mouse) (heavy elephant)
;     (on elephant sparrow)
;     (squashed sparrow)
;     (small sparrow) (on elephant mouse)
;     (squashed mouse) (small mouse)
;     (portable sparrow) (big whale)
;     (portable mouse) (big elephant)
;     (sad elephant) (light sparrow)
;     (heavy whale)}
```

As with the other examples, the matcher performs most of the processing (in this 
case using a `defmatch` construct and `mfor*` in `apply-rule`) while other functions 
collate results, etc.

```clojure
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
```

## Applying State-Changing Operators

In this example we consider how to apply the kind of state changing operators 
that are used in some planning systems. Broadly we adapt a representation 
borrowed from PDDL for use with a STRIPS style solver. The operators are 
specified in terms of their preconditions and their effects. We use tuples to 
capture state information. The following tuples, for example, describe a simple 
state in which some (animated) agent `R` is at a table is holding nothing and a 
book is on the table.

```clojure
#{(at R table)
  (on book table)
  (holds R nil)
  (path table bench)
  (manipulable book)
  (agent R)
  }
```

In order to generalise an operator (so it can be used with different agents, 
objects and in various locations) it is necessary to specify it using variables, 
in this case matcher variables. An operator which describes a "pickup" activity 
for an agent and which can be used to produces a new state (new tuples) could be 
described as follows:

```clojure
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
```

The operator is map with three components:

  * a set of preconditions which must be satisfied in order for the operator to be used
  * a set of tuples to add to an existing state when producing a new state 
  * a set of tuples to delete from an existing state

To apply this kind of operator specification we extract patterns from the 
operator then use `mfind*`:

```clojure
(defn apply-op
  [state {:keys [pre add del]}]
  (mfind* [pre state]
    (union (mout add)
      (difference state (mout del)))))

(apply-op state1 ('pickup ops))
 
; ?  #{(agent R) (holds R book)
;      (manipulable book)
;      (path table bench) (at R table)}
```

The patterns used by `mfind*` are provided dynamically when `apply-op` is called and 
furthermore the patterns themselves define the semantics of the operators.

Collections of operators are conveniently held in a map, and ordered sequences 
of operator applications can be formed by chaining `apply-op` calls, eg:

```clojure
(def ops
  '{pickup {:pre ((agent ?agent)
                  (manipulable ?obj)
                  (at ?agent ?place)
                  (on ?obj ?place)
                  (holds ?agent nil))
            :add ((holds ?agent ?obj))
            :del ((on ?obj   ?place)
                  (holds ?agent nil))}

    drop {:pre ((at ?agent ?place)
                (holds ?agent ?obj))
          :add ((holds ?agent nil)
                (on ?obj ?place))
          :del ((holds ?agent ?obj))}

    move {:pre ((agent ?agent) 
                (at ?agent ?p1)
                (path ?p1 ?p2))
          :add ((at ?agent ?p2))
          :del ((at ?agent ?p1))}})

(-> state1
  (apply-op ('pickup ops))
  (apply-op ('move ops))
  (apply-op ('drop ops)))

; ? #{(agent R) (manipulable book)
;     (on book bench) (holds R nil)
;     (at R bench) (path table bench)}
```
