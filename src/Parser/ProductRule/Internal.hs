module Parser.ProductRule.Internal where

import Data.Set (Set, member)

-- | Two provided symbols besides user defined data.
data Symbol a = Start -- ^ represents the starting symbol.
              | Null  -- ^ represents the null symbol.
              | T a   -- ^ represents a terminal symbol.
              | NT a  -- ^ represents a non-terminal symbol.
              | UD a  -- ^ represents an undetermined symbol, used internally.
            deriving (Eq, Show, Ord)

type ProductRule t = (Symbol t, [Symbol t])


-- | non-terms -> lhs -> rhs -> product rule
rule :: (Ord a) => Symbol a -> [Symbol a] -> ProductRule a
rule = (,)


-- | non-terms
setT :: (Ord a) => Symbol a -> Set a -> Symbol a
setT (UD x) nts = if x `member` nts then NT x else T x
setT x _ = x
