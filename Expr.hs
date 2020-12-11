module Expr where

-- I don't think there's a reason for let and letrec to be seperate?
-- Like, just make every let a letrec implicitly, as in haskell
data Expr = 
  Int Integer
  | Bool Bool
  | Char Char
  | Lam String Expr
  | App Expr Expr
  | Let String Expr Expr 
  | Letrec String Expr Expr
  | IfThenElse Expr Expr Expr
  | Var String
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
  show (Lam x b)      = "(\\"++ x ++ " -> " ++ show b ++ ")"
  show (App e e')     = "(" ++  show e ++ " " ++ show e' ++ ")"
  show (Let f e b)    = "let " ++ f ++ " = " ++ show e ++ " in " ++ show b
  show (Var string)   = string
  show (Letrec f e b) = "let rec " ++ f ++ " = " ++ show e ++ " in " ++ show b
  show (IfThenElse c t e) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show e
