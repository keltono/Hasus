# Hasus

a hobbyist language intended to basically be eager haskell.


I'm going to be reading (and most likely loosely following) the book [The implementation of functional programming languages by Simon Peyton Jones](https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/ )
as I go along.

Features/TODOs:
- [x] HM type inference
- [x] letrecs/recusion
- [x] Evaluation/Beta reduction
- [x] Parsing
  - [ ] math operators
  - [ ] general infix operators
- [ ] top level binds (ability to define functions not in a let expression)
  - currently just evaluates one expression
- [ ] Lists
- [ ] Tuples
- [ ] Algebriac data types
- [ ] Pattern Matching
- [ ] Haskell-style module system
- [ ] tail call optimization 
- [ ] [...]
- [ ] Typeclasses 
- [ ] Monadic IO


misc. feature ideas:
  - it would be somewhat nice if you could use the same name in a pattern match in order to enforce equality, as you would in prolog.
    I guess you would have to do this after implementing typeclasses, and restrict this feature to only things that implement Eq.
    (Since high-order unification is undecidable, and not really sensical in a functional context without dependent types).

Honestly I haven't used haskell enough to find any pain points that seem addressable and not dealt with by compiler extensions. 
Nothing really bugs me about the language.
Interested other people's issues with the language that still involve remaining purely functional, and keeping HM types.

Shout out to [plzoo]( https://github.com/andrejbauer/plzoo ) and [elaborationzoo]( https://github.com/AndrasKovacs/elaboration-zoo ), both of which were used as references for various parts of implementation.
