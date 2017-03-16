module Data.Formula
  ( Formula(..)
  , Variable(..)
  , fromEither
  , toEither
  , eval
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

update :: Eq a => (a -> b) -> a -> b -> a -> b
update f a b a' = if a == a' then b else f a'

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

powerset :: Eq a => [a] -> [[a]]
powerset (a:s) = if a `elem` s then ps else ps ++ map (a:) ps where
  ps = powerset s
powerset _ = [[]]

gen1 :: Int -> [Int]
gen1 size = [0..(size-1)]

gen2 :: Int -> [[Int]]
gen2 = powerset . gen1

genStore1 :: Eq v1 => Int -> [v1] -> [v1 -> Int]
genStore1 size = foldUpd undefined where
  foldUpd f (a:as) = [ upd
    | v1 <- genStore1' a f
    , upd <- foldUpd v1 as
    ]
  foldUpd _ []     = []
  genStore1' var1 g = [ update g var1 res | res <- gen1 size]



genStore2 :: Eq v2 => Int -> [v2] -> [v2 -> [Int]]
genStore2 size = foldUpd undefined where
  foldUpd f (a:as) = [ upd
    | v2 <- genStore2' a f
    , upd <- foldUpd v2 as
    ]
  foldUpd _ []     = []
  genStore2' var2 g = [ update g var2 res | res <- gen2 size]

eval
  :: (Eq v1, Eq v2)
  => Int -> (v1 -> Int) -> (v2 -> [Int]) -> Formula v1 v2 -> Bool
eval _ _ _ FFalse                = False
eval _ _ _ FTrue                 = True
eval size v1 v2 (Neg f)          = not (eval size v1 v2 f)
eval _ v1 _ (x := y)             = v1 x == v1 y
eval _ v1 _ (x :/= y)            = v1 x /= v1 y
eval _ v1 _ (x :< y)             = v1 x < v1 y
eval size v1 v2 (f1 :& f2)       = eval size v1 v2 f1 && eval size v1 v2 f2
eval size v1 v2 (f1 :| f2)       = eval size v1 v2 f1 || eval size v1 v2 f2
eval size v1 v2 (f1 :=> f2)      
  = not (eval size v1 v2 f1) || eval size v1 v2 f2
eval size v1 v2 (Ex (Var1 v) f)  = any (\v1upd -> eval size v1upd v2 f)
  [ update v1 v res | res <- gen1 size]
eval size v1 v2 (Ex (Var2 v) f)  = any (\v2upd -> eval size v1 v2upd f)
  [ update v2 v res | res <- gen2 size]
eval size v1 v2 (All (Var1 v) f) = all (\v1upd -> eval size v1upd v2 f)
  [ update v1 v res | res <- gen1 size]
eval size v1 v2 (All (Var2 v) f) = all (\v2upd -> eval size v1 v2upd f)
  [ update v2 v res | res <- gen2 size]
eval _ v1 v2 (a `In` as)         = v1 a `elem` v2 as
eval _ v1 v2 (a `NotIn` as)      = v1 a `notElem` v2 as

solutions :: (Eq v1, Eq v2) => [v1] -> [v2] -> Formula v1 v2 -> [([Int],[[Int]])]
solutions vars1 vars2 f = [ (map v1 vars1, map v2 vars2)
  | size <- [1..]
  , v1 <- genStore1 size vars1
  , v2 <- genStore2 size vars2
  , eval size v1 v2 f
  ]



