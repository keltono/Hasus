module Typing.Unify where
import Typing.Type
import Typing.Subst
import Control.Monad.Except

--most general unifier
mgu :: (Read e, MonadError e m) => Type -> Type -> m Subst
mgu (TAp l r) (TAp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu r r'
    pure $ s1 @@ s2
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCon i k) (TCon i' k') 
    | i == i' && k == k' = pure nullSubst
mgu t t' = throwError $ read $ "Failed to unify types " ++ show t ++ "and " ++ show t'

--makes a substitution binding a tyvar to a type
--makes a couple checks (kind, occurs) to determine if this is valid.
varBind :: (Read e, MonadError e m) => Tyvar -> Type -> m Subst
varBind u t 
    | t == TVar u      = pure nullSubst
    | u `elem` tvs t   = throwError $ read $ "Type variable " ++ show u ++ "Occurs in type " ++ show t ++ ". Cannot unify."
    | kind u /= kind t = throwError $ read $ "Type variable " ++ show u ++ "has different kind than type" ++ show t ++ ". Cannot unify"
    | otherwise        = pure $ u |-> t

-- mono-directional unification
-- e.g the substitution is only generated for and applied to the left term, not the right term.
match :: (Read e, MonadError e m) => Type -> Type -> m Subst
match (TAp l r) (TAp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu r r'
    s1 `merge` s2
match (TVar u) t | kind u == kind t = varBind u t
match (TCon i k) (TCon i' k') 
    | i == i' && k == k' = pure nullSubst
match t t' = throwError $ read $ "Failed to match types " ++ show t ++ "and " ++ show t'
