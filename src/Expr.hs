{-# LANGUAGE StrictData #-}
module Expr where

type Constructor = String

data Atom = AInt Int | AChar Char
    deriving (Eq)

data Pattern = 
    PVar String
      | PWild
      | PAtom Atom
      | PCon Constructor [Pattern]
    deriving (Eq)

data Expr = 
  --Lit
  Int Int
  | Char Char
--todo
  | Lam String Expr
--ext
  | App Expr Expr
  | Let String Expr Expr 
  | Var String
  | Con Constructor [Expr]
-- easy
  | If Expr Expr Expr
--hm
  | Match Expr [(Pattern,Expr)]
  deriving Eq


instance Show Expr where
  show (Int i)        = show i
  show (Char c)       = show c
  show (Lam x b)      = "(λ"++ x ++ " -> " ++ show b ++ ")"
  show (App e e')     = "(" ++  show e ++ " " ++ show e' ++ ")"
  show (Let f e b)    = "let " ++ f ++ " = " ++ show e ++ " in " ++ show b
  show (If f e b)     = "if " ++ show f ++ " then " ++ show e ++ " else " ++ show b
  show (Var string)   = string
  show (Con cnstr l)  = "(" ++ cnstr ++ concatMap ( (' ':) . show ) l ++ ")"
  show (Match expr p) = "(match " ++ show expr ++ " with\n" ++ concatMap printPatternPair p
                            where printPatternPair (pa,ex) = "    | " ++ show pa ++ " -> " ++ show ex ++ "\n)"

instance Show Atom where
    show (AInt i) = show i 
    show (AChar c) = show c

instance Show Pattern where 
    show (PVar s) = s
    show PWild   = "_"
    show (PAtom a) = show a
    show (PCon c ps) = c ++ concatMap ((' ':) . show) ps
