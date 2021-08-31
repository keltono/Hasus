module Type where
-- import Data.List (foldl', find)
-- import Data.Maybe (mapMaybe)
-- import Data.Tuple (swap)
import Expr

headSafe :: [a] -> Maybe a
headSafe (x:_) = Just x
headSafe []    = Nothing

---- basic Hindley-Milner style type inference
---- Implemented off of memory -- I wanted to see how far I could get without looking up how its done properly
---- As such it ended being pretty hacky/bodgey/kludgey etc
---- Might have some bugs for cases I haven't tested, but it works for all that i have tested.
---- As it stands I think it *is* Hindley Milner 
---- Well, without type constuctors
type Env = [(Expr,Type)]

--typeInfer :: Expr -> Env -> Either String (Env,Type) 
--typeInfer (Lam x expr) env = 
--  typeInfer expr ((Var x,getFreshTyVar env):env) >>= \(env',ty) ->
--  return (env,Arrow (getType env' (Var x)) ty) 

----TODO: add ability to deal with type spesifiers?
--typeInfer (App e1 e2) env = do
--  (env', ty1) <- typeInfer e1 env
--  (env'', ty2) <- typeInfer e2 env'
--  case ty1 of
--    Arrow (Tyvar a) ft2       -> return (env'', typeSub (Tyvar a) ty2 ft2)
     
--    Arrow (Arrow l r) ft2     ->
--      case ty2 of 
--        -- need to be consistently polymorphic
--        -- e.g (a -> b -> b) != (a -> b -> a)
--        -- changes return type.
--        -- e.g (a->a)->a becomes (Int->Int)->Int,not (Int->Int)->a
--        Arrow l' r' -> 
--          let (eq, bindings) = arrowEquiv (Arrow l r) (Arrow l' r') [] in
--            let newTy = foldl' (\t (o,n) -> typeSub o n t) (Arrow l' r') 
--                               $ filter (not . isTyvar . snd) 
--                               $ map swap bindings in
--              let (_,bindings') = arrowEquiv (Arrow l r) newTy [] in
--                if eq
--                  then return (env'', foldl' (\ft2 (org,repl) -> typeSub org repl ft2 ) ft2 bindings' ) 
--                  else Left $  "incompatable function types between argument type'" ++ show (Arrow l r) 
--                               ++ "' and input type '" ++ show (Arrow l' r') ++ "' in  function application \n" ++ show ty1
--          where 
--            arrowEquiv :: Type -> Type -> [(Type,Type)] -> (Bool, [(Type,Type)])
--            arrowEquiv (Arrow l r)  (Arrow l' r') bindings = 
--              let (res,bindings') = arrowEquiv l l' bindings in
--                  if not res then 
--                             (False,bindings)
--                             else 
--                             arrowEquiv r r' bindings'
 
--            -- this will seriously need to be reworked when typeclasses are added, obviously.
--            arrowEquiv (Tyvar a) x bindings = 
--              case find (\(y,_) -> y == Tyvar a) bindings of
--                Just (_,rt) -> (rt == x,bindings)
--                Nothing     -> (True, (Tyvar a, x):bindings)
--            arrowEquiv x (Tyvar a) bindings = 
--              case filter(\(_,y) -> y == Tyvar a) bindings of
--                -- should only be one binding from a concrete type to a tyvar
--                -- x is a concrete type, since the only other option (other than concrete or tyvar)
--                -- is an arrow, which is already covered
--                _:_:_    -> (False,bindings)
--                [(lt,_)] -> (lt == x,bindings)
--                []       -> (True, (x, Tyvar a):bindings)
--            arrowEquiv x y bindings = (x == y, bindings)
--        _ -> error "unreachable case in typechecking"

--    Arrow ft1 ft2             ->
--      case ty2 of 
--       Tyvar _ -> return (replaceType env'' e2 ft1,ft2)
--       _       -> if ft1 == ty2 then
--                                 return (env'', ft2)
--                                 else
--                                 Left $ "Incorrect type in application " ++
--                                   show e1 ++ " : " ++ show (Arrow ft1 ft2) ++ 
--                                     " Applied to " ++
--                                       show e2 ++ " : " ++ show ty2



--    Tyvar a  -> 
--      case e1 of 
--        App f _ -> return (addArg f ty2 env'', Tyvar a)
--        Var f -> return (addArg (Var f) ty2 env'', Tyvar a)



--    _        -> Left $ "non function \'" ++ show e1 ++ ':' : show ty1 ++  "\' applied"

--typeInfer (Var x) env =
--  if isInEnv env (Var x) then 
--                         return (env,getType env (Var x)) 
--                         else 
--                         -- TODO maybe add a seperate env for things defined outside this expr?
--                         -- Would potentially lead to smarter tyvars
--                         Left $ "Variable " ++ x ++ " not in scope"
--                         -- let ty = getFreshTyVar env in return ((Var x,ty):env,ty)
---- TODO: I think it will try to extend a function type that is let bound, which is obviously bad
---- I think a clause in add arg should be able to fix this?
---- well, and changing the type of add arg to a just/either
--typeInfer (Let f x b) env = do
--  (_,ty) <- typeInfer x ((Var f,getFreshTyVar env):env)
--  typeInfer b ((Var f, ty):env)

---- We have this nice function type inference, why not use it?
---- typeInfer (IfThenElse c t e) env = 
----   typeInfer iteAppArgs iteAppEnv
----     where
----       iteAppArgs :: Expr 
----       iteAppArgs =
----         App (App (App itename c) t) e
----       iteAppEnv :: Env
----       iteAppEnv = 
----         let newTyvar = getFreshTyVar env in
----             ((itename,Arrow Boolean (Arrow newTyvar (Arrow newTyvar newTyvar))):env)
----       itename = getFreshVar env
  

--typeInfer (Char _) env = return (env, Character)
--typeInfer (Int _) env  = return (env, Integer)

----helper functions
----assumes ($1,_) `elem` $3
--addArg :: Expr -> Type -> Env -> Env
--addArg (App f _) t env = addArg f t env
---- this is such a terrible hack. 
--addArg (Lam x b) t env = 
--  let oldType = getType env (Lam x b) in
--      replaceType env (Lam x b) (update oldType t) 
--        where 
--          update :: Type -> Type -> Type
--          update (Arrow x y) t = Arrow x (update y t)
--          update x           t = Arrow t x
--addArg (Var x ) t env = 
--  let oldType = getType env (Var x) in
--      replaceType env (Var x) (update oldType t) 
--        where 
--          update :: Type -> Type -> Type
--          update (Arrow x y) t = Arrow x (update y t)
--          update x           t = Arrow t x
--addArg _ _ _ = error "misapplication of addArg"

--isTyvar :: Type -> Bool
--isTyvar (Tyvar _) = True
--isTyvar _         = False

--isInEnv :: Env -> Expr -> Bool
--isInEnv env expr = 
--  case filter (\(x,_) -> x == expr) env of
--    [] -> False
--    _  -> True

--getType :: Env -> Expr -> Type 
--getType env expr = 
--  snd $ head $ filter (\(x,_) -> x == expr) env

----substitute $1 with $2 in $3
--typeSub :: Type -> Type -> Type -> Type
--typeSub t t' (Arrow x y) = 
--  Arrow (typeSub t t' x) (typeSub t t' y)
--typeSub t t' t'' = if t == t'' then t' else t''

--getTyvarString :: Type  -> Maybe Char
--getTyvarString (Tyvar x) = Just x
--getTyvarString _         = Nothing

--getFreshTyVar :: Env -> Type
--getFreshTyVar e = 
--  Tyvar larger 
--    where
--      -- succ '`' == 'a'
--      larger = succ $ foldl' max '`' $ mapMaybe (getTyvarString . snd) e
--getFreshVar :: Env -> Expr
--getFreshVar env = 
--  let vars = filter isVar env in
--      Var (foldl' longest "f" (map (\(Var x,_) -> x) vars) ++ "'")
--    where
--      isVar (Var _,_) = True
--      isVar _       = False
--      longest :: String -> String -> String
--      longest x y = if length x > length y then x else y

  
  

--replaceType :: Env -> Expr -> Type -> Env
--replaceType env expr t = 
--  map (\(ex,ty) -> if ex==expr then (ex,t) else (ex,ty)) env

--ti :: Expr -> Env -> Maybe Type
--ti expr env  = 
--  case typeInfer expr env of 
--    Left   _     -> Nothing
--    Right (_,ty) -> Just ty

---- for testing
--testEnv :: Env
--testEnv = [(Var "+", Arrow Integer (Arrow Integer Integer)), (Var "y", Integer)]
--polyTestEnv :: Env
--polyTestEnv = [(Var "first", Arrow (Tyvar 'a') (Arrow (Tyvar 'b') (Tyvar 'a'))), (Var "y", Integer)]

--testLetExpr :: Expr
--testLetExpr = Let "f" (Lam "x" (Var "x")) (App (Var "f") (Int 2))

--letEnv :: Env
--letEnv = [(Var "ici", Arrow Integer (Arrow Character Integer)), (Var "y", Integer)]

--testLetExpr2 :: Expr
--testLetExpr2 = Let "f" (Lam "x" (Var "x")) (App (App (Var "ici") (App (Var "f") (Int 2))) (App (Var "f") (Char 'l')))

---- no longer wrong! :), used to infer to 'a, now infers to Integer
--wrong :: Expr
--wrong = App (Lam "x" (App (App (Var "x") (Int 1)) (Int 2))) (Var "+")
--wrong' :: Expr
--wrong' = Lam "x" (App (App (Var "x") (Int 1)) (Int 2))

--letrecTest :: Expr
--letrecTest = Let "f" (Lam "x" (App (Var "f") (Var "x"))) (App (Var "f") (Int 2))
