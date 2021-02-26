# Spec 
### A quick informal syntax/semantics to provide some structure.

## Syntax
* OCaml with a haskell sheen
* whitespace insensitive
* top level declarations start with "def", serving the same function as "val" in sml or "let" in ocaml
* Types are annotated with ":"
* Types of functions can be annotated in another line before the function (as in haskell), and values can be annotated inline
    *  arguments are not annotated directly. Do it in the separate function line or don't do it.

Here is a (potentially ambiguous) grammar of expressions in hasus. 
The implementation handles any issues of precedence or ambiguity that may arise from this exact grammar, but this is the gist.
This will be expanded upon later, e.g, with the addition of imports when a module system is created.
The notation is EBNF.

```
<top>    ::= 
        | <decl>
        | <typeDecl>
        | <typeAnnotation>
        

<typeDecl> ::= "data" name "=" <typeDef>

<typeDef>  ::= Capitalname { <type> } [ "|" <typeDef> ]

<type>     ::= 
             | "[" <type> "]" 
             | "(" <type> "," <type> ")"
             | <type> "->" <type>
             | "(" <type> ")"
             | "Int"
             | "Char"
             | Capitalname
             | character

<typeAnnotation> ::= name ":" <type>

<decl>   ::= "def" name "=" <expr>
        
<expr>   ::=
          <lambda>
        | <let>
        | <app>
        | <match>
        | <atom>
        
<lambda> ::= ("\" | "λ") name "->"  <expr>

<let>    ::= "let" name "=" <expr> "in" <expr>

<app>    ::= <expr> <expr>

<match>  ::= "match" <expr> "with" [ "|" ] <pattern> -> <expr> { "|" <pattern> -> <expr> }

<atom>   ::= int | decimal | <string> | <char> | "(" <expr> ")"

<string> ::= '"' {character} '"'

<char>   ::= "'" [character] "'"
```

## Semantics
Strict evaluation.
`main` function is interpreted if a file is intended to be interpreted.
No other differences between Hasus and a subset of OCaml/Haskell for the time being.
