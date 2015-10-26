{-# LANGUAGE ScopedTypeVariables #-}

module Parser.PredictorTable where

import Parser.ProductionRule (Rule, Symbol(..), RuleMap)
import Parser.ProductionRule.Internal (unsetSym, isT)
import Control.Arrow (second)
import Data.Set as S (Set, difference, fromList, toList)
import Data.Maybe (catMaybes, mapMaybe, fromJust)
import Data.Map as M (Map, fromList, lookup, empty, toList, member, insert)
import Control.Monad.State (State, runState, state, put, get)

type Cache a = Map (Symbol a, Int) [Symbol a]
type WithCache a b = State (Cache a) b

lookupCache :: (Ord a) => Symbol a -> Int -> WithCache a (Maybe [Symbol a])
lookupCache s n = do
  cache <- get
  return $ M.lookup (s,n) cache


putCache :: (Ord a) => Symbol a -> Int -> [Symbol a] -> WithCache a [Symbol a]
putCache a n rhs = do
   cache <- get
   put $ M.insert (a,n) rhs cache
   return rhs


nthTs :: (Ord a)
      => Symbol a
      -> Int                    -- ^ `n`-th symbols
      -> RuleMap a              -- ^ rules
      -> WithCache a [Symbol a] -- ^ n-th symbols
nthTs (T a) 1 _ = return [T a]
nthTs (T _) _ _ = return []
nthTs a n rs = f =<< lookupCache a n
   where
      f (Just ls) = return ls
      f Nothing   = concat <$> sequence [ nthTsR rhs n rs | rhs <- fromJust $ M.lookup a rs ]
         >>= putCache a n


nthTsR :: (Ord a)
       => [Symbol a] -- ^ rhs
       -> Int        -- ^ nth
       -> RuleMap a  -- ^ rules
       -> WithCache a [Symbol a]
nthTsR [] _ _ = return []
nthTsR _  0 _ = return []
nthTsR (r:rs) n rules = do
   a <- nthTs r n rules
   b <- nthTsR rs (n-1) rules
   return $ a ++ b

chooseRule :: (Ord a)
           => Symbol a   -- ^ LHS non-terminal symbol A to derive from
           -> [a]        -- ^ next input terminal symbols
           -> RuleMap a  -- ^ production rule map
           -> [Symbol a] -- ^ correct RHS of A to derive to
chooseRule lhs ins rules = chooseRuleR 1 (fromJust $ M.lookup lhs rules) (map T ins) rules


chooseRuleR :: (Ord a)
            => Int          -- ^ current n-th look ahead
            -> [[Symbol a]] -- ^ current candidate RHS that have the same 0-n lookahead symbols
            -> [Symbol a]   -- ^ next input terminal symbols
            -> RuleMap a    -- ^ production rule map
            -> [Symbol a]   -- ^ correct candidate RHS to derive to
chooseRuleR _ [candidate] _ _ = candidate
chooseRuleR n candidates (i:is) rules =
   chooseRuleR
      (n+1)
      (filter (\rhs -> i `elem` fst (syms rhs)) candidates)
      is
      rules
   where
      syms rhs = runState (nthTsR rhs n rules) M.empty
