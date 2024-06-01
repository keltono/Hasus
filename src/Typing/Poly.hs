module Typing.Poly where

import Typing.Type
import Typing.Class
import Typing.Subst
import Data.Map (fromList)
import Control.Monad.Except

data Scheme = 
    Forall [Kind] (Qual Type)
        deriving (Eq,Show)

instance Substitutable Scheme where
    apply s (Forall ks ts) = Forall ks (apply s ts)
    tvs (Forall _ ts) = tvs ts

quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = 
    Forall ks (apply s qt)
        where
            vs'= [v| v <- tvs qt, v `elem` vs]
            ks = map kind vs'
            s = fromList $ zip vs' (map TGen [0..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

data Assump = Id :>: Scheme

instance Substitutable Assump where
    apply s (i :>: sc) = i :>: apply s sc
    tvs (_ :>: sc) = tvs sc


findFromAssump :: (Read e, MonadError e m) => Id -> [Assump] -> m Scheme
findFromAssump i [] = throwError $ read $ "Failed to find unbound identifier " ++ i ++ " in list of assumptions"
findFromAssump i ((i' :>: sc) : as) = if i == i' then return sc else findFromAssump i as
