module Expr where

type Constructor = String

data Expr = 
  Int Integer
  | Bool Bool
  | Char Char
  | Lam String Expr
  | App Expr Expr
  | Let String Expr Expr 
  | Var String
  | Con Constructor [Expr]
  deriving Eq

data Type = 
  Integer
    | Character
    | Boolean
    | Arrow Type Type
    | Tyvar Char 
    deriving Eq

instance Show Type where
  show Integer     = "Integer"
  show Boolean     = "Boolean"
  show Character   = "Character"
  show (Tyvar a)   = '\'':a:"" --borrowing ocaml syntax
  show (Arrow a b) = '(' : show a  ++ " -> " ++ show b ++ ")"

instance Show Expr where
  show (Int i)        = show i
  show (Char c)       = show c
  show (Bool b)       = show b
  show (Lam x b)      = "(λ"++ x ++ " -> " ++ show b ++ ")"
  show (App e e')     = "(" ++  show e ++ " " ++ show e' ++ ")"
  show (Let f e b)    = "let " ++ f ++ " = " ++ show e ++ " in " ++ show b
  show (Var string)   = string
  show (Con cnstr l)  = "(" ++ cnstr ++ concatMap ((\x -> " " ++ x ++ " "). show ) l ++ ")"
