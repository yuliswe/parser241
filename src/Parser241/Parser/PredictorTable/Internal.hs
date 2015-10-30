module Parser241.Parser.PredictorTable.Internal where


import Parser241.Parser.ProductionRule (Rule, Symbol(..), RuleMap)
import Parser241.Parser.ProductionRule.Internal (unsetSym, isT)
import Control.Arrow (second)
import Data.Set as S (Set, difference, fromList, toList)
import Data.Maybe (catMaybes, mapMaybe, fromJust)
import Data.Map.Lazy as M (Map, fromList, lookup)


nthTs :: (Ord a)
      => Symbol a   -- ^ LHS
      -> Int        -- ^ `n`-th symbols
      -> RuleMap a  -- ^ rules
      -> [Symbol a] -- ^ n-th symbols
nthTs (T a) 1 _ = [T a]
nthTs (T _) _ _ = []
nthTs EOF _ _ = []
nthTs a n rs = do
   let rhsLs = fromJust $ M.lookup a rs
   rhs <- rhsLs
   nthTsR rhs n rs


nthTsR :: (Ord a)
       => [Symbol a] -- ^ rhs
       -> Int        -- ^ nth
       -> RuleMap a  -- ^ rules
       -> [Symbol a]
nthTsR [] _ _ = []
nthTsR _  0 _ = []
nthTsR (r:rs) n rules = nthTs r n rules ++ ifNullable r ++ nthTsR rs (n-1) rules
  where
    ifNullable r
      | nullable r rules && not (null rs) = nthTsR rs n rules
      | otherwise = []


nullable :: (Ord a)
         => Symbol a
         -> RuleMap a
         -> Bool
nullable Null _ = True
nullable EOF  _ = False
nullable (T _) _ = False
nullable lhs rules = elem Null (nthTs lhs 1 rules) || all (`nullableR` rules) (fromJust $ M.lookup lhs rules)


nullableR :: (Ord a)
         => [Symbol a]
         -> RuleMap a
         -> Bool
nullableR rhs rules = all (`nullable` rules) rhs


chooseRule :: (Ord a, Show a)
           => Symbol a   -- ^ LHS non-terminal symbol A to derive from
           -> [Symbol a] -- ^ next input terminal symbols
           -> RuleMap a  -- ^ production rule map
           -> [Symbol a] -- ^ correct RHS of A to derive to
chooseRule lhs ins rules = chooseRuleR 1 (fromJust $ M.lookup lhs rules) ins rules


chooseRuleR :: (Ord a, Show a)
            => Int          -- ^ current n-th look ahead
            -> [[Symbol a]] -- ^ current candidate RHS that have the same 0-n lookahead symbols
            -> [Symbol a]   -- ^ next input terminal symbols
            -> RuleMap a    -- ^ production rule map
            -> [Symbol a]   -- ^ correct candidate RHS to derive to
chooseRuleR _ [candidate] _ _ = candidate
chooseRuleR n candidates (i:is) rules
   | null candidates' = error $ "chooseRuleR: No production rule can be applied. Next input: " ++ show i
                                 ++ "\nCandidates: " ++ show candidates
   | otherwise = chooseRuleR (n+1) candidates' is rules
   where candidates' = filter (\x -> i `elem` nthTsR x n rules) candidates
chooseRuleR n candidates [] rules = error $ "chooseRuleR: More than one production rule can be applied at EOF. The syntax is ambigous."
                                             ++ "\nCandidates: " ++ show candidates

