{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Eval where
import Expr 
import Type
import Data.Maybe (fromJust)

data Val 
  = VVar String
  | VApp Val Val
  | VLam String (Val -> Val)
  | VBool Bool
  | VInt Int
  | VChar Char
    
type Env' = [(String,Val)]

eval :: Env' -> Expr -> Val
eval env = \case
  Int i            -> VInt i
  Bool b           -> VBool b
  Char c           -> VChar c
  Lam x t          -> VLam x (\u -> eval ((x,u):env) t)
  App l r          -> eval env l `app` eval env r
  Let f x b        -> eval ((f,eval env x):env) b
  IfThenElse c t e -> 
    case eval env c of
      VBool True  -> eval env t
      VBool False -> eval env e
  Letrec f !x b     -> 
    --TODO this is sus
    -- this might not loop forever b/c lazyness? not sure
    let env'  = (f, eval env' x):env in
        eval env' b


  Var x    -> fromJust $ lookup x env 
  
app :: Val -> Val -> Val
app (VLam _ t) x = t x
-- not sure why/if this is necessary...
app l r        = VApp l r

fresh :: [String] -> String -> String
fresh _ "_" = "_"
fresh ns x   =
  if x `elem` ns then
            fresh ns (x ++ "'")
            else
             x

quote :: [String] -> Val -> Expr
quote ns = \case
  VVar x                 -> Var x
  VApp t u               -> App (quote ns t) (quote ns u)
  VLam (fresh ns -> x) t -> Lam x (quote (x:ns) (t (VVar x)))
  VBool b                -> Bool b
  VChar c                -> Char c
  VInt i                 -> Int i
ti' = ti

nf' :: Env' -> Expr -> Expr
nf' env = quote (map fst env) . eval env

nf :: Expr -> Env' -> Expr
nf a b = nf' b a

interpret :: Expr -> Env' -> Either String Expr
interpret expr env = 
  case typeInfer expr [] of
    Left e -> Left e
    Right _ -> Right $ nf expr env
