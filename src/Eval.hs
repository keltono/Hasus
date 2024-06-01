{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GADTs #-}
module Eval where
import Expr
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Applicative (liftA2)

funcDefToMatch :: [(String, [Pattern], Expr)] -> Expr
funcDefToMatch ds = foldr Lam (Match (Con "[]" argvars) $ zip (map (PCon "[]") pats) exprs) args
    where
        pats :: [[Pattern]]
        exprs :: [Expr]
        (pats,exprs) = foldr (\(_,p,e) (ps,es) -> (p:ps,e:es)) ([],[]) ds
        args = map show [90090..90090+length (head pats) - 1]
        argvars = map Var args 

enumerate :: [a] -> [(a,Int)]
enumerate x = zip x [0..length x - 1]

--- Implemented in a psuedo-nanopass style
--- First step: Turn Lets into lambdas
-- data Expr1 where
--   Int1 :: Int -> Expr1
--   Char1 :: Char -> Expr1
--   Lam1 :: String -> Expr1 -> Expr1
--   App1 :: Expr1 -> Expr1 -> Expr1
--   Var1 :: String -> Expr1
--   Con1 :: Constructor -> [Expr1] -> Expr1
--   If1 :: Expr1 -> Expr1 -> Expr1 -> Expr1
--   Match1 :: Expr1 -> [(Pattern, Expr1)] -> Expr1
--   deriving Eq

-- --- I'm just going to write all of this boilerplate for now :)

-- letTransform :: Expr -> Expr1
-- letTransform (Int i)      = Int1 i
-- letTransform (Char c)     = Char1 c
-- letTransform (Lam s e)    = Lam1 s (letTransform e)
-- letTransform (App l r)    = letTransform l `App1` letTransform r
-- letTransform (Var s)      = Var1 s
-- letTransform (Con s es)   = Con1 s $ map letTransform es
-- letTransform (If c t e)   = If1 (letTransform c) (letTransform t) (letTransform e)
-- letTransform (Match e bs) = Match1 (letTransform e) ((fmap . fmap) letTransform bs)
-- letTransform (Let s v e)  = Lam1 s (letTransform e) `App1` letTransform v
-- data Expr = 
--   Int !Int
--   | Char !Char
--   | Lam !String !Expr
--   | App !Expr !Expr
--   | Let !String !Expr !Expr 
--   | Var !String
--   | Con !Constructor ![Expr]
--   | If !Expr !Expr !Expr
--   | Match !Expr ![(Pattern,Expr)]
--   deriving Eq

--- Second step: Transform away Match Expressions into nested Ifs
-- For now do directly and sequentially, can do optimizations later
-- Also: Introduce primitive "op" expression, in this case for adding operation for comparing fields in ADTs
-- Also: Add "String" For internal use, Strings at the top level are lists of chars
--data Expr2 where
--  Int2 :: Int -> Expr2
--  Char2 :: Char -> Expr2
--  Lam2 :: String -> Expr2 -> Expr2
--  App2 :: Expr2 -> Expr2 -> Expr2
--  Var2 :: String -> Expr2
--  Con2 :: Constructor -> [Expr2] -> Expr2
--  If2 :: Expr2 -> Expr2 -> Expr2 -> Expr2
--  Let2 :: String -> Expr2 -> Expr2 -> Expr2
--  Op2 :: String -> [Expr2] -> Expr2
--  String2 :: String -> Expr2
--  deriving Eq

---- Not particularly effecient, but shouldn't matter?
--matchTransform :: Expr -> Expr2
--matchTransform (Int i)      = Int2 i
--matchTransform (Char c)     = Char2 c
--matchTransform (Lam s e)    = Lam2 s (matchTransform e)
--matchTransform (App l r)    = matchTransform l `App2` matchTransform r
--matchTransform (Var s)      = Var2 s
--matchTransform (Con s es)   = Con2 s $ map matchTransform es
--matchTransform (If c t e)   = If2 (matchTransform c) (matchTransform t) (matchTransform e)
--matchTransform (Let s t e)  = Let2 s (matchTransform t) (matchTransform e)
--matchTransform (Match e ts) = foldr (translateCase (matchTransform e) . fmap matchTransform) (Con2 "MatchFailure" []) ts
--    where
--        translateCase :: Expr2 -> (Pattern,Expr2) -> Expr2 -> Expr2
--        translateCase ex (PVar s,v)           _     = Lam2 s v `App2` ex
--        translateCase _  (PWild,v)            _     = v
--        translateCase ex (PAtom (AInt i), v)  later = If2 (Op2 "==" [Int2 i, ex]) v later
--        translateCase ex (PAtom (AChar c), v) later = If2 (Op2 "==" [Char2 c, ex]) v later
--        translateCase ex (PCon c ps, v)       later =
--            If2 (Op2 "==" [Op2 "getConstructor" [ex], String2 c ]) (foldr (transPConElem ex later) v (enumerate ps)) later

--        --similar to above, but we need to handle failure and PCon differently
--        transPConElem :: Expr2 -> Expr2 -> (Pattern,Int) -> Expr2 -> Expr2
--        transPConElem _  _    (PWild,_)           later = later
--        transPConElem ex fail (PAtom (AInt i),_)  later = If2 (Op2 "==" [Int2 i, ex]) later fail
--        transPConElem ex fail (PAtom (AChar c),_) later = If2 (Op2 "==" [Char2 c, ex]) later fail
--        transPConElem ex _    (PVar s,i)          later = Lam2 s later `App2` Op2 "GetNthElem" [Int2 i,ex]
--        transPConElem ex fail (PCon c ps,i)       later =
--            If2 (Op2 "==" [Op2 "getConstructor" [ex], String2 c ])
--                (foldr (transPConElem (Op2 "GetNthElem" [Int2 i,ex]) fail) later (enumerate ps))
--                fail

---- Switch to de Bruijn indicies
--data Expr3 where
--  Int3 :: Int -> Expr3
--  Char3 :: Char -> Expr3
--  Lam3 :: Expr3 -> Expr3
--  Let3 :: Expr3 -> Expr3 -> Expr3
--  App3 :: Expr3 -> Expr3 -> Expr3
--  Var3 :: Int -> Expr3
--  Con3 :: Constructor -> [Expr3] -> Expr3
--  If3 :: Expr3 -> Expr3 -> Expr3 -> Expr3
--  Op3 :: String -> [Expr3] -> Expr3
--  String3 :: String -> Expr3
--  deriving (Show, Eq)

---- Change the start index depending on the amount of things in the pervasive enviroment
--deBruijnRename :: Int -> Map String Int -> Expr2 -> Expr3
--deBruijnRename startIndex state start =
--        case start of
--            (Lam2 s e)    -> deBruijnRename startIndex (Map.insert s startIndex ((+1) <$> state)) e
--            (Let2 s v e)  -> Let3 (deBruijnRename startIndex state v) (deBruijnRename startIndex (Map.insert s startIndex ((+1) <$> state)) e)
--            (Var2 s)      -> Var3 $ fromJust $ Map.lookup s state
--            (Int2 i)      -> Int3 i
--            (Char2 c)     -> Char3 c
--            (String2 s)   -> String3 s
--            (App2 l r)    -> App3 (deBruijnRename startIndex state l) (deBruijnRename startIndex state r)
--            (Con2 s es)   -> Con3 s $ map (deBruijnRename startIndex state) es
--            (If2 c t e)   -> If3 (deBruijnRename startIndex state c) (deBruijnRename startIndex state t) (deBruijnRename startIndex state e)
--            (Op2 n es)    -> Op3 n $ map (deBruijnRename startIndex state) es


--- Convert remaining into A-Normal Form. Doesn't need a new ADT


data Val where
  VVar :: String -> Val
  VApp :: Val -> Val -> Val
  VLam :: String -> (Val -> Val) -> Val
  VCon :: String -> [Val] -> Val
  VInt :: Int -> Val
  VChar :: Char -> Val

type VEnv = Map String Val

eval :: VEnv -> Expr -> Val
eval env = \case
  Int i       -> VInt i
  Char c      -> VChar c
  Con c l     -> VCon c $ map (eval env) l
  Lam x t     -> VLam x (\u -> eval (Map.insert x u env) t)
  App l r     -> eval env l `app` eval env r
  Var x       -> fromJust $ Map.lookup x env
  If _ _ _    -> undefined
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
    patternDoesMatch (VInt i) (PAtom (AInt i'))   = i == i'
    patternDoesMatch (VInt _) _                   = False
    patternDoesMatch (VChar c) (PAtom (AChar c')) = c == c'
    patternDoesMatch (VChar _) _                  = False
    patternDoesMatch _ (PAtom _)                  = False
    patternDoesMatch (VCon c vs) (PCon c' pats)   = c == c' && and (zipWith patternDoesMatch vs pats)
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
-- startEnv :: Env
-- startEnv = [(Var "~", Arrow Int Int),(Var "+",Arrow Int (Arrow Int Int)),(Var "-",Arrow Int (Arrow Int Int)),
--             (Var "*",Arrow Int (Arrow Int Int)),(Var "==",Arrow (Tyvar "a") (Arrow (Tyvar "a") (Constructor "Boolean" [])))]

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
    (VInt xi, VInt yi)   -> if xi == yi then VCon "True" [] else VCon "False" []
    (VChar xi, VChar yi) -> if xi == yi then VCon "True" [] else VCon "False" []
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
-- frac :: Int -> Either String Expr
-- frac arg = interpret (Let "f" (Lam "x" (IfThenElse (App (App (Var "==") (Var "x")) (Int 0)) (Int 1) (App (App (Var "*") (Var "x")) (App (Var "f") (App (App (Var "-") (Var "x")) (Int 1)))))) (App (Var "f") (Int arg))) []
