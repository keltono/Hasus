{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Eval where
import Expr 
import Type
import Data.Maybe (fromJust)
import Control.Applicative (liftA2)

data Val 
  = VVar String
  | VApp Val Val
  | VLam String (Val -> Val)
  | VBool Bool
  | VInt Integer 
  | VChar Char
    
type Env' = [(String,Val)]

eval :: Env' -> Expr -> Val
eval env = \case
  Int i            -> VInt i
  Bool b           -> VBool b
  Char c           -> VChar c
  Lam x t          -> VLam x (\u -> eval ((x,u):env) t)
  App l r          -> eval env l `app` eval env r
  Var x            -> fromJust $ lookup x env 
  IfThenElse c t e -> 
    case eval env c of
      VBool True  -> eval env t
      VBool False -> eval env e
      _           -> error "non-bool in if-then-else condition"
  Let f !x b     -> 
    -- laziness saves the day!
    let env'  = (f, eval env' x):env in
        eval env' b
  
app :: Val -> Val -> Val
app (VLam _ t) x = t x
app l r        = VApp l r

fresh :: [String] -> String -> String
fresh _ "_" = "_"
fresh ns x   =
  if x `elem` ns then
            fresh ns (x ++ "'")
            else
             x

quote :: [String] -> Val -> Either String Expr
quote ns = \case
  VVar x     -> return $ Var x
  VApp t u   -> liftA2 App (quote ns t)  (quote ns u)
  VLam _ _   -> Left "cannot print Lambda" 
  VBool b    -> return $ Bool b
  VChar c    -> return $ Char c
  VInt i     -> return $ Int i

nf :: Env' -> Expr -> Either String Expr
nf env = quote (map fst env) . eval env

interpret :: Expr -> Env' -> Either String Expr
interpret expr env =
  -- TODO maybe make it so it doesn't try to eval if typeInfer fails?
  env' >>= \env'' -> typeInfer expr (env''<>startEnv) >> nf (env<>startEnv') expr 
  where 
    env' :: Either String Env
    env' =
      -- assumption: expressions in the enviroment can only depend on things later in the list
      let (names,values) = unzip env in
        let temp = reverse $  zip (map Var names) $ map (quote names) values  in 
            -- just turned a 5 line function of type [(a,Either b c)] -> Either b [(a,c)]
            -- into "mapM sequence"
            -- monads are cool
            -- A lot of the time it feels like your under layers of monads that you need to dig yourself out of to get to the data
            -- but it's worth it to be able to write highly generic and nice code, i'd say
            mapM sequence temp >>= \temp' -> typeInferBatch temp' []

typeInferBatch :: [(Expr,Expr)] -> [(Expr,Type)] -> Either String Env
typeInferBatch ((var,val):env') env = 
  typeInfer val env >>= \(_,ty) -> typeInferBatch env' ((var,ty):env)
typeInferBatch [] env = return env

startEnv' :: Env' 
startEnv' = [("+",plus),("-",minus),("*",times),("==",eq),("~",neg)]
startEnv :: Env
startEnv = [(Var "~", Arrow Integer Integer),(Var "+",Arrow Integer (Arrow Integer Integer)),(Var "-",Arrow Integer (Arrow Integer Integer)),(Var "*",Arrow Integer (Arrow Integer Integer)),(Var "==",Arrow (Tyvar 'a') (Arrow (Tyvar 'a') Boolean))]

-- built in functions
-- the symbol for negation after parsing is '~'
neg :: Val
neg = VLam "x" (\(VInt xi) -> VInt (-xi))

plus :: Val
plus = VLam "x" (\(VInt xi) ->
       VLam "y" (\(VInt yi) ->
            VInt $ xi + yi
         ))
minus :: Val
minus = VLam "x" (\(VInt xi) ->
        VLam "y" (\(VInt yi) ->
            VInt $ xi - yi
         ))
times :: Val
times = VLam "x" (\(VInt xi) ->
        VLam "y" (\(VInt yi) ->
            VInt $ xi * yi
         ))
eq :: Val
eq = VLam "x" (\x -> VLam "y" (\y ->  
  case (x,y) of 
    (VInt xi, VInt yi)   -> VBool $ xi == yi
    (VBool xi, VBool yi) -> VBool $ xi == yi
    (VChar xi, VChar yi) -> VBool $ xi == yi
    _ -> error "invalid eq"
 ))

frac :: Integer -> Either String Expr
frac arg = interpret (Let "f" (Lam "x" (IfThenElse (App (App (Var "==") (Var "x")) (Int 0)) (Int 1) (App (App (Var "*") (Var "x")) (App (Var "f") (App (App (Var "-") (Var "x")) (Int 1)))))) (App (Var "f") (Int arg))) []
