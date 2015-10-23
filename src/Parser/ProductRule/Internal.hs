module Parser.ProductRule.Internal where

import Data.Set (Set, member)

data Symbol a = Start | Null | NT a | T a | UD a deriving (Eq, Show, Ord)

type ProductRule t = (Symbol t, [Symbol t])
type Token t s = (t, s)


-- | non-terms -> lhs -> rhs -> product rule
rule :: (Ord a) => Symbol a -> [Symbol a] -> ProductRule a
rule = (,)


-- | non-terms
setT :: (Ord a) => Symbol a -> Set a -> Symbol a
setT (UD x) nts = if x `member` nts then NT x else T x
setT x _ = x
