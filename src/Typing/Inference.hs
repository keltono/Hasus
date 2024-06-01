{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This entire typing/inference portion of the code is heavily based on Typing Haskell in Haskell
-- By Mark Peyton Jones. http://web.cecs.pdx.edu/~mpj/thih/thih.pdf
-- Because the code is so similar to the code presented in the literate haskell document above,
-- the code here is less commented than it otherwise should be. For any confusions, see the paper.
module Typing.Inference where

import Typing.Subst
import Typing.Type
import Typing.Unify
import Typing.Class
import Typing.Poly
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad (filterM, zipWithM)
import Data.Bifunctor (first, second)
import Data.List ((\\), partition, union, intersect)

class Instantitate t where
    inst :: [Type] -> t -> t

instance Instantitate Type where
    inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
    inst ts (TGen n)  = ts !! n
    inst _ t = t
instance Instantitate Pred where
    inst ts (IsIn c t) = IsIn c (inst ts t)
instance Instantitate t => Instantitate (Qual t) where
    inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantitate a => Instantitate [a] where
    inst ts = map (inst ts)

-- the type inference monad.
-- keeps track of the current substitution, the int increment for the type variables, and if a failure has occured
-- could replace string with a better error type (that keeps track of more information) as a future improvement
newtype TI a = TI (StateT (Subst, Int) (Except String) a)
    deriving (Functor, Applicative, Monad, MonadState (Subst,Int), MonadError String)


runTI :: TI a -> Either String a
runTI (TI st) = runExcept $ evalStateT st (nullSubst,0)

getSubst :: TI Subst
getSubst = fst <$> get

getInt :: TI Int
getInt = snd <$> get

unify :: Type -> Type -> TI ()
unify t1 t2 = do
    s <- getSubst
    u <- mgu (apply s t1) (apply s t2)
    extendSubst u

modifySubst :: (Subst -> Subst) -> TI ()
modifySubst f = modify' $ first f

modifyInt :: (Int -> Int) -> TI ()
modifyInt f = modify' $ second f

extendSubst :: Subst -> TI ()
extendSubst s' = modifySubst (s'@@)

newTVar :: Kind -> TI Type
newTVar k = do
    n <- getInt
    modifyInt (+1)
    pure $ TVar $ Tyvar ("v" ++ show n) k 

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
    ts <- mapM newTVar ks
    return (inst ts qt)
-- begin the section on actual inference

--inference of literals
data Literal = 
     LitInt Integer
   | LitChar Char
   | LitFrac Rational
   | LitStr String
data Pat = 
    PVar Id
  | PWildCard
  | PLit Literal
  -- the assump represents the constructor, giving both its name and its type
  | PCon Assump [Pat]
  -- represents the a@p thing in haskell, could add later
  -- | PAs Id Pat
data TyExpr =
    Var Id
  | Lit Literal
  | Const Assump
  | Ap TyExpr TyExpr
  | Let BindGroup TyExpr
type FuncDef = ([Pat],TyExpr)

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred],t)

-- inference of literals
--don't need the classEnv or Assumptions
tiLit :: Literal -> TI ([Pred],Type)
tiLit (LitChar _) = pure ([], tChar)
tiLit (LitStr _)  = pure ([], tString)
tiLit (LitInt _)  = newTVar Star >>= \v -> pure ([IsIn "num" v],v)
tiLit (LitFrac _) = newTVar Star >>= \v -> pure ([IsIn "Fractional" v],v)

-- inference of patterns
-- calculates what all of the individual patterns should be (the [Assump] portion of the output), 
-- as well as what the whole thing should match with (The Type portion of the ouput)
tiPat :: Pat -> TI ([Pred], [Assump], Type) 
tiPat (PVar i)  = newTVar Star >>= \v -> pure ([],[i :>: toScheme v],v)
tiPat PWildCard = newTVar Star >>= \v -> pure ([],[],v)
tiPat (PLit l)  = tiLit l >>= \(ps,t) -> pure (ps,[],t)
tiPat (PCon (_ :>: sc) ps) = do
    (ps',as,ts) <- tiPats ps
    t' <- newTVar Star
    (qs :=> t) <- freshInst sc
    unify t (foldr fn t' ts)
    return (ps' ++ qs, as, t')

tiPats :: [Pat] -> TI ([Pred],[Assump],[Type])
tiPats ps = 
    foldr combine ([],[],[]) <$> mapM tiPat ps
        where
            combine (prs,as,t) (prs',as',ts) = (prs ++ prs',as ++ as', t:ts)
-- inference of general exprs
tiTyExpr :: Infer TyExpr Type
tiTyExpr _ as (Var i) = do
    sc <- findFromAssump i as
    (ps :=> t) <- freshInst sc
    return (ps,t)
tiTyExpr _ _ (Const (_ :>: sc)) = do
    (ps :=> t) <- freshInst sc
    return (ps,t)
tiTyExpr _ _ (Lit l) = tiLit l
tiTyExpr ce as (Ap e f) = do
    (ps, te) <- tiTyExpr ce as e
    (ps', tf) <- tiTyExpr ce as f
    t <- newTVar Star
    unify (tf `fn` t) te
    pure (ps ++ ps', t)
tiTyExpr ce as (Let bg e) = do
    (ps, as') <- tiBindGroup ce as bg
    (ps', t) <- tiTyExpr ce (as' ++ as) e
    pure (ps ++ ps',t)

tiFuncDef :: Infer FuncDef Type 
tiFuncDef ce as (pats, e) = do
    (ps,as',ts) <- tiPats pats
    (ps',t) <- tiTyExpr ce (as' ++ as) e
    pure (ps ++ ps', foldr fn t ts)

-- checks relative to a known type
tiFuncDefs :: ClassEnv -> [Assump] -> [FuncDef] -> Type -> TI [Pred]
tiFuncDefs ce as fds t = do
    ls <- mapM (tiFuncDef ce as) fds
    mapM_ (unify t . snd) ls
    pure $ concatMap fst ls

--generalization
-- ideally would just work by quatifying over all the tyvars, but this doesn't account for typeclasses
-- we need to make use of the predicates, simplify them, etc.
split :: ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> TI ([Pred],[Pred])
split ce fs gs ps = do 
    -- do context reduction
    ps' <- reduce ce ps
    -- defer productions that only talk about "fixed" (e.g not free) tyvars
    let (ds,rs) = partition (all (`elem` fs) . tvs) ps'
    -- do defaulting as necessary
    rs' <- defaultedPreds ce (fs ++ gs) rs
    pure (ds,rs \\rs')

-- defaulting. TODO
defaultedPreds :: ClassEnv -> [Tyvar] -> [Pred] -> TI [Pred]
defaultedPreds _ _ _ = pure []

-- Binding groups
type BindGroup = ([Expl], [[Impl]])

--Explicitly typed binding groups
type Expl = (Id, Scheme, [FuncDef])

tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (i,sc,fds) = do
    (qs :=> t) <- freshInst sc
    ps <- tiFuncDefs ce as fds t
    s <- getSubst
    let qs'          = apply s qs
        t'           = apply s t
        fixedTyVars  = tvs (apply s as)
        freeVars     = tvs t' \\ fixedTyVars
        sc'          = quantify freeVars (qs' :=> t')
    ps'  <- filterM (fmap not . entail ce qs') (apply s ps)
    (ds,rs) <- split ce fixedTyVars freeVars ps'
    if sc /= sc' then throwError $ "signature of " ++ i ++ "to general. Inferred " ++ show sc' ++ " but saw " ++ show sc
                 else if not $ null rs
                     then throwError $ "context too weak in typechecking of " ++ show i ++ ". Had the following predicates required: " ++ show rs
                     else pure ds

-- Implcitly typed binding groups
-- not doing monomorphism restriction b/c it seems kinda dumb
type Impl = (Id, [FuncDef])
tiImpls :: Infer [Impl] [Assump]
tiImpls ce as bs = do
    ts <- mapM (\_ -> newTVar Star) bs
    let is = map fst bs
        scs = map toScheme ts
        as' = zipWith (:>:) is scs ++ as
        fds = map snd bs
    pss <- zipWithM (tiFuncDefs ce as') fds ts
    s <- getSubst
    let ps' = apply s (concat pss)
        ts' = apply s ts
        fs  = tvs (apply s as)
        vss = map tvs ts'
        gs  = foldr1 union vss \\ fs
    (ds,rs) <- split ce fs (foldr1 intersect vss) ps'
    let scs' = map (quantify gs . (rs :=>)) ts'
    pure (ds, zipWith (:>:) is scs')

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (es, is) = do
    let as' = [v :>: sc | (v,sc,_) <- es]
    (ps,as'') <- tiSeq tiImpls ce (as' ++ as)  is
    qss <- mapM (tiExpl ce (as ++ as' ++ as'')) es
    return (ps ++ concat qss, as'' ++ as')

tiSeq :: Infer b [Assump] -> Infer [b] [Assump]
tiSeq _ _ _ [] = pure ([],[])
tiSeq ti ce as (bs:bss) = do
    (ps,as') <- ti ce as bs 
    (qs,as'') <- tiSeq ti ce (as' ++ as) bss
    pure (ps ++ qs, as'' ++ as')

-- TODO add defaulting
tiProgram :: ClassEnv -> [Assump] -> [BindGroup] -> Either String [Assump]
tiProgram ce as p = runTI $ do
    (ps, as') <- tiSeq tiBindGroup ce as p
    s <- getSubst
    -- Used in Defaulting
    _ <- reduce ce (apply s ps)
    return $ apply s as'
