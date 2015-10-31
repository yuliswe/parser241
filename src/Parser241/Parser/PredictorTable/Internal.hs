module Parser241.Parser.PredictorTable.Internal where


import Parser241.Parser.ProductionRule (Rule, Symbol(..), RuleMap)
import Parser241.Parser.ProductionRule.Internal (unsetSym, isT)
import Control.Arrow (second)
import Data.Set as S (Set, difference, fromList, toList)
import Data.Maybe (catMaybes, mapMaybe, fromJust, fromMaybe)
import Data.Map.Lazy as M (Map, fromList, lookup)
import Parser241.Debug
import Data.Foldable (foldMap)
import Control.Monad (filterM, when, liftM2)

nthTs :: (Ord a, Show a)
      => Symbol a   -- ^ LHS
      -> Int        -- ^ `n`-th symbols
      -> RuleMap a  -- ^ rules
      -> WithLog [Symbol a] -- ^ n-th symbols
nthTs (T a) 1 _ = return [T a]
nthTs (T _) _ _ = return []
nthTs EOF _ _ = return []
nthTs Null _ _ = return []
nthTs a n rs = do
   rhsLs <- lookupRHSLs a rs
   concat <$> sequence [ nthTsR rhs n rs | rhs <- rhsLs ]


nthTsR :: (Ord a, Show a)
       => [Symbol a] -- ^ rhs
       -> Int        -- ^ nth
       -> RuleMap a  -- ^ rules
       -> WithLog [Symbol a]
nthTsR [] _ _ = return []
nthTsR _  0 _ = return []
nthTsR (r:rs) n rules = do
   a <- nthTs r n rules
   b <- nthTsR rs (n-1) rules
   c <- ifNullable r
   return $ a ++ c ++ b
   where
      ifNullable r = do
         bool <- nullable r rules
         if bool && not (null rs) then nthTsR rs n rules
         else return []


nullable :: (Ord a, Show a)
         => Symbol a
         -> RuleMap a
         -> WithLog Bool
nullable Null _ = return True
nullable EOF  _ = return False
nullable (T _) _ = return False
nullable lhs rules = do
   rhs  <- nthTs lhs 1 rules
   rhsLs <- lookupRHSLs lhs rules
   bool <- and <$> mapM (`nullableR` rules) rhsLs
   return $ elem Null rhs || bool


nullableR :: (Ord a, Show a)
         => [Symbol a]
         -> RuleMap a
         -> WithLog Bool
nullableR rhs rules = and <$> mapM (`nullable` rules) rhs


chooseRule :: (Ord a, Show a)
           => Symbol a   -- ^ LHS non-terminal symbol A to derive from
           -> [Symbol a] -- ^ next input terminal symbols
           -> RuleMap a  -- ^ production rule map
           -> WithLog [Symbol a] -- ^ correct RHS of A to derive to
chooseRule EOF _ _ = throwError "You called chooseRule with EOF as the LHS."
chooseRule lhs ins rules = do
   x <- lookupRHSLs lhs rules
   chooseRuleR 1 x ins rules


chooseRuleR :: (Ord a, Show a)
            => Int          -- ^ current n-th look ahead
            -> [[Symbol a]] -- ^ current candidate RHS that have the same 0-n lookahead symbols
            -> [Symbol a]   -- ^ next input terminal symbols
            -> RuleMap a    -- ^ production rule map
            -> WithLog [Symbol a]   -- ^ correct candidate RHS to derive to
chooseRuleR _ [candidate] _ _ = return candidate
chooseRuleR n candidates (i:is) rules = do
   candidates' <- filterM (\x -> elem i <$> nthTsR x n rules) candidates
   when (null candidates') (throwError $ "chooseRuleR: No production rule can be applied. Next input: " ++ show i++ "\nCandidates: " ++ show candidates)
   chooseRuleR (n+1) candidates' is rules
chooseRuleR n candidates [] rules = throwError $ "chooseRuleR: More than one production rule can be applied at EOF. The syntax is ambigous."
                                             ++ "\nCandidates: " ++ show candidates


lookupRHSLs :: (Ord a, Show a) => Symbol a -> RuleMap a -> WithLog [[Symbol a]]
lookupRHSLs a rules =
   case M.lookup a rules of
      Nothing -> throwError $ "LHS '"++show a++"' does not exist in "++show rules
      Just x  -> return x
