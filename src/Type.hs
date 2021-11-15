-- based on stephen diehl's hindley milner type inference article in the "write you a haskell" series
-- extended to allow ADTs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Type where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Expr

newtype TVar = TV String
  deriving (Eq,Show,Ord)
-- functions (at the type level) could theoretically be viewed as a special case of constructors, but we choose the make the distinction.
data Type 
  = TVar TVar
  | TConst String
  | TCon Constructor [Type]
  | TArr Type Type
  deriving (Eq,Show)

-- polymorphic types. If the list of tyvars is empty, then the type is monomorphic
data Scheme = Forall [TVar] Type
  deriving (Eq,Show)

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
  deriving (Semigroup, Monoid, Eq, Show)

testEnv :: TypeEnv
testEnv = TypeEnv (Map.fromList [("Just", Forall [TV "a"] (TArr (TVar (TV "a")) (TCon "Maybe" [TVar (TV "a")]))), ("None", Forall [] (TCon "Maybe" []))])

newtype Unique = Unique { count :: Int }
  deriving (Eq,Show)
data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  deriving (Eq,Show)

type Infer = ExceptT TypeError (State Unique)

type Subst = Map.Map TVar Type

unInfer :: Infer a -> Either TypeError a
unInfer m =  (evalState . runExceptT) m $ Unique {count = 0 }

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TConst a)     = TConst a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (TCon c ts)    = TCon c $ apply s <$> ts
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TConst{}       = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (TCon _ ts)    = foldr (\x s -> ftv x `Set.union` s) Set.empty ts
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

-- TODO: move this into a standard library
defaultConstructorTypes :: Map.Map String Scheme
defaultConstructorTypes = Map.fromList [("True",Forall [] $ TConst "Bool"),("False",Forall [] $ TConst "Bool"), 
  ("Cons", Forall [TV "a"] $ TArr (TVar (TV "a")) (TCon "Maybe" [TVar (TV "a")])), ("Nil", Forall [TV "a"] $ TCon "Maybe" [TVar (TV "a")])]

extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env
  
fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1 }
  return $ TVar $ TV (show $ count s)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t


bind ::  TVar -> Type -> Infer Subst
bind a t | t == TVar a     = return nullSubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ Map.singleton a t

unify :: Type -> Type -> Infer Subst
unify (l `TArr` r) (l' `TArr` r') = do
  s1 <- l `unify` l'
  s2 <- apply s1 r `unify` apply s1 r'
  return (s2 `compose` s1)
unify (TConst a) (TConst a') | a == a' =  pure nullSubst 
unify (TCon c a) (TCon c' a') | c == c' = foldM (\s (ts,ts') -> (apply s ts `unify` apply s ts') >>= \s' -> pure $ s' `compose` s ) nullSubst $ zip a a'
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify t t' =  throwError $ UnificationFail t t'

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

lookupEnv :: TypeEnv -> String -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x = 
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> instantiate s >>= \t -> return (nullSubst, t)


infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env = \case 
    Var x -> lookupEnv env x
    Int _  -> return (nullSubst, TConst "Int")
    Char _  -> return (nullSubst, TConst "Char")
    Lam x e ->  do
      tv <- fresh
      let env' = env `extend` (x, Forall [] tv)
      (s1, t1) <- infer env' e
      return (s1, apply s1 tv `TArr` t1)
    App e1 e2 -> do
      tv <- fresh
      (s1, t1) <- infer env e1
      (s2, t2) <- infer (apply s1 env) e2
      s3       <- unify (apply s2 t1) (TArr t2 tv)
      return (s3 `compose` s2 `compose` s1, apply s3 tv)
    Let x e1 e2 -> do
      (s1, t1) <- infer env e1
      let env' = apply s1 env
          t'   = generalize env' t1
      (s2, t2) <- infer (env' `extend` (x, t')) e2
      return (s1 `compose` s2, t2)
    Match e os -> do
      (s1, t1) <- infer env e
      --TODO typecheck the patterns
      let es = map snd os
      -- get type/subsitution from unifying each branch together
      (s2,t2) <- foldM (\(s,t) b -> infer (apply s env) b >>= \(s',t') -> unify (apply s' t) t' >>= \s'' -> return (s'', apply s'' t')) (s1,t1) es
      s3 <- unify (apply s2 t1) t2
      return (s3 `compose` s2 `compose` s1,t2)
    Con c bs -> 
      infer env $ foldl App (Var c) bs
      -- infer env $ foldr App (Var c) bs 

    -- If cond tr fl -> do
    --   (s1, t1) <- infer env cond
    --   (s2, t2) <- infer env tr
    --   (s3, t3) <- infer env fl
    --   s4 <- unify t1 typeBool
    --   s5 <- unify t2 t3
    --   return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t2)
