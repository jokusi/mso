module Data.Formula
  ( Formula(..)
  ) where

infixl 3 :&,:|
infixr 2 :=>
infix 4 :=,:/=,`In`

data Variable v1 v2 = Var1 v1 | Var2 v2 deriving (Show, Read, Eq)

fromEither :: Either a b -> Variable a b
fromEither (Left a) = Var1 a
fromEither (Right b) = Var2 b

toEither :: Variable a b -> Either a b
toEither (Var1 a) = Left a
toEither (Var2 b) = Right b

data Formula v1 v2
  = FFalse
  | FTrue
  | Neg (Formula v1 v2)
  | v1 := v1
  | v1 :/= v1
  | v1 :< v1
  | Formula v1 v2 :& Formula v1 v2
  | Formula v1 v2 :| Formula v1 v2
  | Formula v1 v2 :=> Formula v1 v2
  | Ex (Variable v1 v2) (Formula v1 v2)
  | All (Variable v1 v2) (Formula v1 v2)
  | In v1 v2
  | NotIn v1 v2
  deriving (Eq, Show, Read)


data Var = P | P' deriving (Show, Read, Eq)
data Status = Idle | Wait | Crit | Idle' | Wait' | Crit'
  deriving (Show, Read, Eq)

blocking :: Var -> Formula Var Status
blocking p = Ex (Var1 P') (P' `In` Crit :| (p :< P' :& P' `In` Wait))

trans :: Var -> Formula Var Status
trans p
  =  (p `In` Idle :& p `In` Idle' :=> Ex (Var1 P') (P' `In` Wait))
  :& (p `In` Idle :& p `In` Wait' :=> All (Var1 P') (p :/= P' :=> P' `In` Idle'))
  :& (p `In` Idle :& p `In` Crit' :=> FFalse)
  :& (p `In` Wait :& p `In` Idle' :=> FFalse)
  :& (p `In` Wait :& p `In` Wait' :=> blocking p)
  :& (p `In` Wait :& p `In` Crit' :=> Neg (blocking p))
  :& (p `In` Crit :& p `In` Idle' :=> FTrue)
  :& (p `In` Crit :& p `In` Wait' :=> FFalse)
  :& (p `In` Crit :& p `In` Crit' :=> FFalse)


