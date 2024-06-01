{-# LANGUAGE StrictData #-}
module Typing.Type where

type Id = String

data Kind  = Star | Kfun Kind Kind
             deriving (Show, Eq, Ord)

data Type  = TVar Tyvar | TCon Id Kind | TAp Type Type | TGen Int
             deriving (Show, Eq)

data Tyvar = Tyvar Id Kind
             deriving (Show, Eq, Ord)

class HasKind t where
  kind :: t -> Kind
instance HasKind Tyvar where
  kind (Tyvar _ k) = k
instance HasKind Type where
  kind (TCon _ k) = k
  kind (TVar u)  = kind u
  kind (TGen _)  = error "tried to get kind of a generic type variable outside of a scheme."
  kind (TAp t r) = case kind t of
                     (Kfun _ k) -> k
                     -- should never happen
                     Star       -> error $ "kind mismatch for " ++ show t ++ ". Expected a higher kinded type, got a type of kind * when applied to " ++ show r
 

tUnit :: Type
tUnit    = TCon  "()" Star
tChar :: Type
tChar    = TCon "Char" Star
tInt :: Type
tInt     = TCon "Int" Star
tInteger :: Type
tInteger = TCon "Integer" Star
tFloat :: Type
tFloat   = TCon "Float" Star
tDouble :: Type
tDouble  = TCon "Double" Star

tList :: Type
tList    = TCon  "[]" (Kfun Star Star)
tArrow :: Type
tArrow   = TCon "(->)" (Kfun Star (Kfun Star Star))
tTuple2 :: Type
tTuple2  = TCon "(,)" (Kfun Star (Kfun Star Star))

tString :: Type
tString  = list tChar

infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TAp (TAp tArrow a) b

list :: Type -> Type
list  = TAp tList 

pair :: Type -> Type -> Type
pair a  = TAp (TAp tTuple2 a)

