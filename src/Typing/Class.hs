{-# LANGUAGE LambdaCase #-}
module Typing.Class where
import Typing.Type
import Typing.Subst
import Typing.Unify (mgu, match)
import Data.List (nub)
import Control.Monad.Except
import Data.Maybe (isJust, isNothing)

data Pred = IsIn Id Type
    deriving (Eq)
instance Show Pred where
    show (IsIn i t) = show i ++ " has type " ++ show t

data Qual t = [Pred] :=> t
    deriving (Eq,Show)

-- The name of each superclass and each instance declaration
type Class = ([Id],[Inst])
-- A list of potential req. instances :=> the instance itself 
type Inst = Qual Pred

data ClassEnv = ClassEnv { classes :: Id -> Maybe Class, defaults :: [Type] }

instance Substitutable Pred where
    apply s (IsIn i t) = IsIn i (apply s t)
    tvs (IsIn _ t) = tvs t

instance Substitutable t => Substitutable (Qual t) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    tvs (ps :=> t) = nub $ tvs ps ++ tvs t

mguPred, matchPred :: (Read e, MonadError e m) => Pred -> Pred -> m Subst

mguPred = liftPred mgu
matchPred = liftPred match

liftPred :: (Read e, MonadError e m) => (Type -> Type -> m Subst) -> Pred -> Pred -> m Subst
liftPred m (IsIn i t) (IsIn i' t') =
    if i == i' then m t t' else throwError $ read "classes did not Match"

superClasses :: ClassEnv -> Id -> [Id]
superClasses ce i = case classes ce i of 
                      Just (is, _) -> is
                      Nothing -> []
instances :: ClassEnv -> Id -> [Inst]
instances ce i = case classes ce i of 
                      Just (_, its) -> its
                      Nothing -> []

modifyClass :: ClassEnv -> Id -> Class -> ClassEnv
modifyClass ce i c = ce { classes = \j -> if i == j then Just c else classes ce j }

initialClassEnv :: ClassEnv
initialClassEnv = ClassEnv {classes = const Nothing, defaults = [tInt,tDouble]}

type EnvTransformer m =  ClassEnv -> m ClassEnv

infixr 5 <:>
(<:>) :: (Read e, MonadError e m) => EnvTransformer m -> EnvTransformer m -> EnvTransformer m
(f <:> g) ce = f ce >>= g

addClass :: (Read e, MonadError e m) => Id -> [Id] -> EnvTransformer m
addClass i is ce
    -- class already defined
    | isJust (classes ce i) = throwError $ read $ "Class " ++ i ++ "already defined"
    -- superclass not defined
    | any (isNothing . classes ce) is =  throwError $ read $ "A superclass is not defined for class " ++ i ++ ", expected one of " ++ show is
    | otherwise = pure (modifyClass ce i (is,[]))

addCoreClasses :: (Read e, MonadError e m) => EnvTransformer m
addCoreClasses = 
    addClass "Eq" [] 
    <:> addClass "Ord" ["Eq"]
    <:> addClass "Show" []
    <:> addClass "Read" []
    <:> addClass "Functor" []
    <:> addClass "Monad" ["Functor"]

addNumClasses :: (Read e, MonadError e m) => EnvTransformer m
addNumClasses = 
    addClass "Num" ["Eq", "Show"]
    <:> addClass "Fractional" ["Num"]
    <:> addClass  "Integral" ["Num"]

addPreludeClasses :: (Read e, MonadError e m) => EnvTransformer m
addPreludeClasses = addCoreClasses <:> addNumClasses

addInst :: (Read e, MonadError e m) =>  [Pred] -> Pred -> EnvTransformer m
addInst ps p@(IsIn i _) ce = 
    if isNothing (classes ce i) 
        then throwError $ read "Class not defined, cannot add instance"
        else tryError (mapM_ (mguPred p) qs) >>= 
            \case
                Left _ -> pure $ modifyClass ce i c
                Right _ -> throwError $ read "overlapping instances"
    where
        ts = instances ce i
        qs = [q| (_ :=> q) <- ts]
        c = (superClasses ce i, (ps :=> p) : ts)
exampleInsts :: (Read e, MonadError e m) => EnvTransformer m
exampleInsts = addPreludeClasses
    <:> addInst [] (IsIn "Ord" tChar)
    <:> addInst [] (IsIn "Ord" tInt)
    
--if predicate p holds, then so do all the predicates in the output
-- this is dtermined by superclass information
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) =
    p : concat [bySuper ce (IsIn i' t) | i' <- superClasses ce i]

byInst :: (Read e, MonadError e m) => ClassEnv -> Pred -> m [Pred]
byInst ce p@(IsIn i _) = head <$> mapM tryInst (instances ce i)
    where
        tryInst :: (Read e, MonadError e m) => Inst -> m [Pred]
        tryInst (ps :=> h) = matchPred h p >>= \u -> pure $ map (apply u) ps

entail  :: (Read e, MonadError e m) => ClassEnv -> [Pred] -> Pred -> m Bool
entail ce ps p = 
    (byInst ce p >> pure True) `catchError` (\_ -> pure sups)
         where sups = any (elem p . bySuper ce) ps

--context reduction 
isHnf :: Pred -> Bool
isHnf (IsIn _ t) = hnf t
    where 
        hnf (TCon _ _) = False
        hnf (TAp l _) = hnf l
        hnf _ = True

toHnfs :: (Read e, MonadError e m) => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = 
    concat <$> mapM (toHnf ce) ps 

toHnf :: (Read e, MonadError e m) => ClassEnv -> Pred -> m [Pred]
toHnf ce p =
    if isHnf p 
       then pure [p]
       else byInst ce p >>= toHnfs ce

simplify :: (Read e, MonadError e m) => ClassEnv -> [Pred] -> m [Pred]
simplify ce = loop []
    where 
        loop rs [] = pure rs
        loop rs (p:ps) = do
              o <- entail ce (rs ++ ps) p
              if o then loop rs ps else loop (p:rs) ps
reduce :: (Read e, MonadError e m) => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps =  toHnfs ce ps >>= simplify ce 
