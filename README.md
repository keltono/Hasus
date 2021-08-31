# Hasus

a hobbyist language intended to be very similar to an eager haskell.

## Syntax
At the moment, syntax is similar to haskell, with some things borrowed from OCaml (and the "def" keyword from python).
Notably, hasus is not whitespace-sensitive.
There is no spec right now, but the [examples folder](examples/) should be resonably comprehensive in showing off the syntax.
## Semantics
As said above, it is itended to very similar to haskell,
except eager instead of lazy, and with various minor tweaks.
Currently, the language is more or less [HM](https://en.wikipedia.org/wiki/Hindley–Milner_type_system) without type constuctors. 
There are three built in sorts (Integers, Chars, Bools), the function type, and polymorphic types as described in [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley–Milner_type_system).
The semantics of the language will be extended (roughly) according to the todo-list below.

I'm going to be reading (and most likely loosely following) the book [The implementation of functional programming languages by Simon Peyton Jones](https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/ )
as I go along.

Before now I was doing things pretty ad-hoc, which was fun, but now its time to do things "properly"
Currently Typechecking is broken while I extend the core language.
Check [the spec](spec.md) for more exact details.

Features/TODOs:
- [x] letrecs/recusion
- [x] Evaluation/Beta reduction
- [x] Parsing
  - [x] math operators
  - [x] general infix operators
- [x] Data Constructors
- [x] Pattern Matching
- [x] Parsing for Pattern Matching 
- [x] top level binds (ability to define functions not in a let expression)
- [x] Parsing for Data Constructors
- [ ] List Syntax
- [ ] Tuple Syntax
- [ ] Multi-function lets / mutual recursion
- [ ] HM type inference
- [ ] User defined (algebriac) types
    - You can technically do this currently, if only because the interpreter doesn't check if your constructors have been defined
- [ ] Type annotations
- [ ] Algorithm-W type inference
- [ ] Haskell-style module system
- [ ] user-defined infix operators
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


