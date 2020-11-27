module Expr where

data Expr = 
  Int Int
  | Char Char
  | Lam String Expr
  | App Expr Expr
  | Let String Expr Expr 
  | Var String
  deriving Eq

data Type = 
  Integer
    | Character
    | Arrow Type Type
    | Tyvar Char 
    deriving Eq

instance Show Type where
  show Integer     = "Integer"
  show Character   = "Character"
  show (Tyvar a)   = '\'':a:"" --borrowing ocaml syntax
  show (Arrow a b) = '(' : show a  ++ " -> " ++ show b ++ ")"

instance Show Expr where
  show (Int i) = show i
  show (Char c) = show c
  show (Lam x b) = "(\\"++ x ++ " -> " ++ show b ++ ")"
  show (App e e') = "(" ++  show e ++ " " ++ show e' ++ ")"
  show (Let f e b) = "let " ++ f ++ " = " ++ show e ++ " in " ++ show b
  show (Var string) = string
