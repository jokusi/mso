module Data.Formula
  ( Formula(..)
  ) where

data Formula v1 v2
  = Var (Either v1 v2)
  | Neg (Formula v1 v2)
  | Formula v1 v2 :& Formula v1 v2
  | Formula v1 v2 :| Formula v1 v2
  | Ex (Either v1 v2) (Formula v1 v2)
  | All (Either v1 v2) (Formula v1 v2)
  | In v1 v2
  | NotIn v1 v2
  deriving (Eq, Show, Read)


