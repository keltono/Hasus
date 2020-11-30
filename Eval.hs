{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Eval where
import Expr 
import Type
import Data.Maybe (fromJust)
-- import Debug.Trace
import System.IO.Unsafe
import GHC.Stack

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
    -- laziness saves the day!
    -- (well, mostly. This is obviously not that difficult to simulate in ocaml)
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
nf' :: Env' -> Expr -> Expr
nf' env = quote (map fst env) . eval env

nf :: Expr -> Env' -> Expr
nf a b = nf' b a

interpret :: Expr -> Env' -> Either String Expr
interpret expr env =
  let !_ = unsafePerformIO currentCallStack in  env' >>= \env' -> typeInfer expr (env'<>startEnv) >> return (nf expr (env<>startEnv'))
  where 
    env' :: Either String Env
    env' = 
      -- assumption: expressions in the enviroment can only depend on things later in the list
      let (names,values) = unzip env in
        let temp = reverse $  zip (map Var names) $ map (quote names) values  in 
            typeInferBatch temp [] >>= \e -> return e
          
typeInferBatch ((var,val):env') env = 
  typeInfer val env >>= \(_,ty) -> typeInferBatch env' ((var,ty):env)
typeInferBatch [] env = return env

startEnv' :: Env' 
startEnv' = [("+",plus),("-",minus),("*",times),("==",eq)]
startEnv :: Env
startEnv = [(Var "+",Arrow Integer (Arrow Integer Integer)),(Var "-",Arrow Integer (Arrow Integer Integer)),(Var "*",Arrow Integer (Arrow Integer Integer)),(Var "==",Arrow (Tyvar 'a') (Arrow (Tyvar 'a') Boolean))]

plus :: Val
plus = VLam "x" (\x ->
       VLam "y" (\y ->
         -- breaks on quotation w/o this
         case (x,y) of 
           (VInt xi, VInt yi) -> VInt $ xi + yi
           (_,_)              -> VInt   42
         ))
minus :: Val
minus = VLam "x" (\x ->
       VLam "y" (\y ->
         -- breaks on quotation w/o this
         case (x,y) of 
           (VInt xi, VInt yi) -> VInt $ xi - yi
           (_,_)              -> VInt   43
         ))
times :: Val
times = VLam "x" (\x ->
       VLam "y" (\y ->
         -- breaks on quotation w/o this
         case (x,y) of 
           (VInt xi, VInt yi) -> VInt $ xi * yi
           (_,_)              -> VInt   44
         ))
eq :: Val
eq = VLam "x" (\x -> VLam "y" (\y ->  
  case (x,y) of 
    (VInt xi, VInt yi)   -> VBool $ xi == yi
    (VBool xi, VBool yi) -> VBool $ xi == yi
    (VChar xi, VChar yi) -> VBool $ xi == yi
    -- should only ever happen during quotation
    (_, _)               -> VBool   False
 ))

frac arg = interpret (Letrec "f" (Lam "x" (IfThenElse (App (App (Var "==") (Var "x")) (Int 0)) (Int 1) (App (App (Var "*") (Var "x")) (App (Var "f") (App (App (Var "-") (Var "x")) (Int 1)))))) (App (Var "f") (Int arg))) []
