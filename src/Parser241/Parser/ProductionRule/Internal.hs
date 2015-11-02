module Parser241.Parser.ProductionRule.Internal where

import Data.Set (Set, member)
import Data.Map as M (Map)

-- | Two provided symbols besides user defined data.
data Symbol a = Start -- ^ represents the starting symbol.
              | Null  -- ^ represents the null symbol.
              | T a   -- ^ represents a terminal symbol.
              | NT a  -- ^ represents a non-terminal symbol.
              | UD a  -- ^ represents an undetermined symbol, used internally.
              | EOF   -- ^ EOF
            deriving (Eq, Show, Ord)

type Rule t = (Symbol t, [[Symbol t]])
type RuleMap a = Map (Symbol a) [[Symbol a]]


isT :: Symbol a -> Bool
isT (T a) = True
isT _     = False

isTerm :: Symbol a -> Bool
isTerm (T _) = True
isTerm EOF = True
isTerm Null = True
isTerm _ = False

isNonTerm :: Symbol a -> Bool
isNonTerm (NT _) = True
isNonTerm Start = True
isNonTerm _ = False


isNP :: Symbol a -> Bool
isNP Start = True
isNP Null = True
isNP EOF = True
isNP _ = False

-- | non-terms -> lhs -> rhs -> product rule
rule :: (Ord a) => Symbol a -> [[Symbol a]] -> Rule a
rule Start rhsLs = (Start, map (++[EOF]) rhsLs)
rule a rhsLs = (a, rhsLs)


-- | non-terms
setSym :: (Ord a) => Symbol a -> Set a -> Symbol a
setSym (UD x) nts = if x `member` nts then NT x else T x
setSym x _ = x


unsetSym :: Symbol a -> Maybe a
unsetSym (T  a) = Just a
unsetSym (NT a) = Just a
unsetSym (UD a) = Just a
unsetSym _ = Nothing


