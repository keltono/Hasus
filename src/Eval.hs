{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
--TODO remove this line soon
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Eval where
import Expr 
import Type
import Data.Maybe (fromJust)
import Control.Applicative (liftA2)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')
import Control.Monad.State.Lazy 
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
  Match e ps  -> evalMatch (eval env e) ps
    where
    evalMatch :: Val -> [(Pattern,Expr)] -> Val
    evalMatch (VApp _ _) _  = undefined
    evalMatch (VLam _ _) _  = error "Can't pattern match on function"
    evalMatch (VVar s)   l  = evalMatch (fromJust $ Map.lookup s env) l
    evalMatch val l = eval env' e
        where
            (p,e) = head (filter (patternDoesMatch val . fst) l)
            (_,env') = runState (computePatternBindings $ return (p,val)) env

    patternDoesMatch :: Val -> Pattern -> Bool
    patternDoesMatch _ PWild                      = True
    patternDoesMatch _ (PVar _)                   = True
    patternDoesMatch (VBool b) (PAtom (ABool b')) = b == b'
    patternDoesMatch (VBool _) _                  = False
    patternDoesMatch (VInt i) (PAtom (AInt i'))   = i == i'
    patternDoesMatch (VInt _) _                   = False
    patternDoesMatch (VChar c) (PAtom (AChar c')) = c == c'
    patternDoesMatch (VChar _) _                  = False
    patternDoesMatch _ (PAtom _)                  = False
    patternDoesMatch (VCon c vs) (PCon c' pats)   = c == c' && foldl' (&&) True (zipWith patternDoesMatch vs pats)
    patternDoesMatch _ (PCon _ _)                 = False

    computePatternBindings :: State VEnv (Pattern,Val) -> State VEnv (Pattern,Val)
    -- Creates a state which eventually becomes the set of bindings of things in the pattern to things in the expression being matched on
    -- these bindings are then used to eval the expression to the right of the matched pattern
    -- This one is kinda sick
    computePatternBindings s = do
        start <- s
        state <- get
        case start of 
          (PCon _ pats, VCon _ vals) -> mapM_ (computePatternBindings . return) (zip pats vals) >> return start
          (PCon _ _, _)              -> undefined
          (PVar s, v)                -> put (Map.insert s v state) >> return start
          (PAtom _, _)               -> s
          (PWild, _)                 -> s

  
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
      _ -> undefined
    )
tl :: Val
tl = VLam "x" (\case
      VCon "Cons" (_:[VCon x y]) -> VCon x y
      VCon "Nil"  []    -> error "tail used on empty list"
      _ -> undefined
    )


exampleList :: Expr
exampleList = Con "Cons" [Int 1, Con "Cons" [Int 2, Con "Nil" [] ]]

testMatch  :: Expr
testMatch = Match exampleList [(PCon "Cons" [PVar "h", PWild], App (App (Var "+") (Var "h")) (Int 9) ), (PCon "Nil" [], Int 42)]

testMatch'  :: Expr
testMatch' = Match (Con "Nil" []) [(PCon "Cons" [PVar "h", PWild], App (App (Var "+") (Var "h")) (Int 2) ), (PCon "Nil" [], Int 42)]

testMatch''  :: Expr
testMatch'' = Match exampleList [(PCon "Cons" [PVar "h", PCon "Cons" [PVar "h'", PWild]], App (App (Var "+") (Var "h")) (Var "h'") ), (PCon "Nil" [], Int 99)]

testMatch'''  :: Expr
testMatch''' = Match exampleList [(PCon "Nil" [], Int 42)]

one :: Expr 
one = App (Var "head") exampleList

twoSingleton :: Expr 
twoSingleton = App (Var "tail") exampleList
-- frac :: Integer -> Either String Expr
-- frac arg = interpret (Let "f" (Lam "x" (IfThenElse (App (App (Var "==") (Var "x")) (Int 0)) (Int 1) (App (App (Var "*") (Var "x")) (App (Var "f") (App (App (Var "-") (Var "x")) (Int 1)))))) (App (Var "f") (Int arg))) []
