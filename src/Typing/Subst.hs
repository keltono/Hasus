module Typing.Subst where
import qualified Data.Map.Strict as Map
import Typing.Type
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.List (nub, intersect)
import Control.Monad.Except

type Subst = Map Tyvar Type

class Substitutable t where
    apply :: Subst -> t -> t
    tvs :: t -> [Tyvar]

instance Substitutable Type where
    apply s t@(TVar v)   = fromMaybe t (Map.lookup v s)
    apply s (TAp l r)    = TAp (apply s l) (apply s r)
    apply _ t            = t

    tvs (TAp l r) = tvs l ++ tvs r
    tvs (TVar u)  = [u]
    tvs _         = []

instance Substitutable a => Substitutable [a] where
    apply s = map (apply s)
    tvs = nub . concatMap tvs

nullSubst :: Subst 
nullSubst = Map.empty

(|->) :: Tyvar -> Type -> Subst
u |-> t = Map.singleton u t

-- composition of substitutions. Left biased.
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = apply s1 <$> s2 `Map.union` s1

-- communative subsitution composition.
-- can fail.
merge :: (Read e, MonadError e m) => Subst -> Subst -> m Subst
--TODO better errors, probably
merge s1 s2 = if agree then pure $ s1 `Map.union` s2 else throwError $ read "Failed to merge substitutions"
    where agree = all (\v -> apply s1 (TVar v ) == apply s2 (TVar v ))
            (Map.keys s1 `intersect` Map.keys s2)
