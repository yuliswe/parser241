module Parser241.Parser.ProductionRule.Internal where

import Data.Set (Set, member)
import Data.Map as M (Map)

-- | Two provided symbols besides user defined data.
data Symbol a = Start -- ^ represents the starting symbol.
              | Null  -- ^ represents the null symbol.
              | T a   -- ^ represents a terminal symbol.
              | NT a  -- ^ represents a non-terminal symbol.
              | UD a  -- ^ represents an undetermined symbol, used internally.
            deriving (Eq, Show, Ord)

type Rule t = (Symbol t, [[Symbol t]])
type RuleMap a = Map (Symbol a) [[Symbol a]]


isT :: Symbol a -> Bool
isT (T a) = True
isT _     = False


-- | non-terms -> lhs -> rhs -> product rule
rule :: (Ord a) => Symbol a -> [[Symbol a]] -> Rule a
rule = (,)


-- | non-terms
setSym :: (Ord a) => Symbol a -> Set a -> Symbol a
setSym (UD x) nts = if x `member` nts then NT x else T x
setSym x _ = x


unsetSym :: Symbol a -> Maybe a
unsetSym (T  a) = Just a
unsetSym (NT a) = Just a
unsetSym (UD a) = Just a
unsetSym _ = Nothing


