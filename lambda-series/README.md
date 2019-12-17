# Creating your own language with the lambda calculus as the core.

## Implementation

Build and install using stack via `stack install`.

Get stack here: http://docs.haskellstack.org/en/stable/README/
(will automatically pull in GHC and all the requirements for you).

## Example programs

See `programs/simple.lam` and `test.lam`.

## Rough idea for the list of topics:
    
 - [x] Session 1: Intro to the syntax and concepts of the lambda calculus, 
    including operational semantics.
    
 - [x]  Session 2: A first implementation: parsing, printing, and interpreting 
    the lambda calculus (written in Haskell) [video (Kent only)](https://kent.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=7d6df6a1-c4b7-42e2-a4b3-aae100d89b17)
 - [x]  Session 3: Church encodings (natural numbers, booleans, recursion) and some playing with the implementation. [video (Kent only)](https://kent.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=6ff8fedf-125b-40bf-86f3-aae8010b4383)
 - [x]  Session 4: Formally defining a type system via the simply-typed lambda 
    calculus
 - [x]  Session 5: Algorithmically defining a type system: type checking and 
    type inference [video (Kent only)](https://kent.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=86ff0fcd-1702-4c92-8102-aaf600fa82a8)
 - [x] Session 6: PCF and PCF typing
 - [x] Session 7: The Curry-Howard correspondence in more detail (types vs. logic)
 - [x] Session 8: Call-By-Value vs. Call-By-Name reduction.
 - [x] Session 9: Polymorphism (System F)
 - [x] Session 10: Polymorphic lambda-calculus typing and Theorems for Free.
 
 ---------------------------------------------
 
 - [ ] Session Y: Implementing ML-style polymorphic types (Hindley-Milner algorithm 
    and related).
