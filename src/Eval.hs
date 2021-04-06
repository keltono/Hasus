{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Eval where
import Expr 
import Type
import Data.Maybe (fromJust)
import Control.Applicative (liftA2)
import Data.Map (Map)
import qualified Data.Map as Map

data Val 
  = VVar String
  | VApp Val Val
  | VLam String (Val -> Val)
  | VCon String [Val]
  | VBool Bool
  | VInt Integer 
  | VChar Char
    
type VEnv = Map String Val

eval :: VEnv -> Expr -> Val
eval env = \case
  Int i       -> VInt i
  Bool b      -> VBool b
  Char c      -> VChar c
  Con c l     -> VCon c $ map (eval env) l
  Lam x t     -> VLam x (\u -> eval (Map.insert x u env) t)
  App l r     -> eval env l `app` eval env r
  Var x       -> fromJust $ Map.lookup x env 
  Let f !x b  -> 
    -- laziness saves the day!
    let env'  = Map.insert f (eval env' x) env in
        eval env' b
  
app :: Val -> Val -> Val
app (VLam _ t) x = t x
app l r        = VApp l r

quote ::  Val -> Either String Expr
quote  = \case
  VVar x    -> return $ Var x
  VApp t u  -> liftA2 App (quote t) (quote u)
  VLam _ _  -> Left "cannot print Lambda" 
  -- just mapping quote leads to a [Either String Expr] instead of a
  -- Either String [Expr]
  -- mapping, then turning t m a into m t b is exactly what mapM is for.
  -- We then use fmap to apply the constructor to the inside expression, 
  -- leading to Either String Expr (Where the Expr is a Con)
  -- A somewhat verbose explanation, but lines like this can be somewhat instruitable without some context
  VCon c l  -> Con c <$> mapM quote l
  VBool b   -> return $ Bool b
  VChar c   -> return $ Char c
  VInt i    -> return $ Int i

nf :: VEnv -> Expr -> Either String Expr
nf env = quote . eval env

interpret :: Expr -> VEnv -> Either String Expr
interpret expr env =
   nf (env<>startVEnv) expr 

inter :: Expr -> Either String Expr
inter = nf startVEnv



startVEnv :: VEnv 
startVEnv = Map.fromList [("+",plus),("-",minus),("*",times),("==",eq),("~",neg),("head", hd), ("tail", tl)]
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

hd :: Val
hd = VLam "x" (\case
      VCon "Cons" (h:_) -> h
      VCon "Nil"  []    -> error "head used on empty list"
    )
tl :: Val
tl = VLam "x" (\case
      VCon "Cons" (_:[VCon x y]) -> VCon x y
      VCon "Nil"  []    -> error "tail used on empty list"
    )


exampleList :: Expr
exampleList = Con "Cons" [Int 1, Con "Cons" [Int 2, Con "Nil" [] ]]

one :: Expr 
one = App (Var "head") exampleList

twoSingleton :: Expr 
twoSingleton = App (Var "tail") exampleList
-- frac :: Integer -> Either String Expr
-- frac arg = interpret (Let "f" (Lam "x" (IfThenElse (App (App (Var "==") (Var "x")) (Int 0)) (Int 1) (App (App (Var "*") (Var "x")) (App (Var "f") (App (App (Var "-") (Var "x")) (Int 1)))))) (App (Var "f") (Int arg))) []
