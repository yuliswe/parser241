module Parser241.Parser.PredictorTable.Internal where


import Parser241.Parser.ProductionRule (Rule, Symbol(..), RuleMap, isTerm, isNonTerm)
import Parser241.Parser.ProductionRule.Internal (unsetSym, isT)
import Control.Arrow (second)
import Data.Set as S (Set, difference, fromList, toList)
import Data.Maybe (catMaybes, mapMaybe, fromJust, fromMaybe)
import Data.Map.Lazy as M (Map, fromList, lookup, toList)
import Parser241.Debug
import Data.Foldable (foldMap)
import Control.Monad (filterM, when, liftM2)

fstTs :: (Ord a, Show a)
      => Symbol a   -- ^ LHS
      -> RuleMap a  -- ^ rules
      -> WithLog [Symbol a] -- ^ n-th symbols
fstTs lhs = nthTs lhs 1

fstTsR :: (Ord a, Show a)
       => [Symbol a] -- ^ rhs
       -> RuleMap a  -- ^ rules
       -> WithLog [Symbol a]
fstTsR rhs = nthTsR rhs 1


nthTs :: (Ord a, Show a)
      => Symbol a   -- ^ LHS
      -> Int        -- ^ `n`-th symbols
      -> RuleMap a  -- ^ rules
      -> WithLog [Symbol a] -- ^ n-th symbols
nthTs a n rs
   | isTerm a = return [a | n == 1]
   | isNonTerm a = do
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
   chooseRuleLog $ printf "Check if %s is nullable." (show lhs)
   rhs  <- nthTs lhs 1 rules
   rhsLs <- lookupRHSLs lhs rules
   bool <- or <$> mapM (`nullableR` rules) rhsLs
   bool <- return $ elem Null rhs || bool
   chooseRuleLog $ printf "Result is %s." (show bool)
   return bool


nullableR :: (Ord a, Show a)
         => [Symbol a]
         -> RuleMap a
         -> WithLog Bool
nullableR rhs rules = all (Null `elem`) <$>
   mapM (\lhs -> nthTs lhs 1 rules) rhs


followR :: (Ord a, Show a)
        => Symbol a              -- ^ followed
        -> Symbol a              -- ^ lhs
        -> [Symbol a]            -- ^ rhs symbols
        -> RuleMap a
        -> WithLog [Symbol a]
followR a lhs [] rules
   | a == lhs = return []
   | otherwise = follow lhs rules
followR a lhs r@(s:_) rules
   | isNonTerm a && isTerm s = return [s]
   | isNonTerm a && isNonTerm s = do
      a <- fstTsR r rules
      bool <- nullableR r rules
      b <- follow lhs rules
      parserLog $ "followR " ++ show b
      return $ if bool then a ++ b else a


follow :: (Ord a, Show a) => Symbol a -> RuleMap a -> WithLog [Symbol a]
follow a rules = concat <$> sequence
   [ followR a lhs p rules | (lhs, p) <- ls ]
   where ls = [ (lhs, p) | (lhs, rhsLs) <- M.toList rules, rhs <- rhsLs, p <- partsAfter a rhs ]


partsAfter :: (Eq a) => Symbol a -> [Symbol a] -> [[Symbol a]]
partsAfter _ [] = []
partsAfter x ls
   | null a = []
   | otherwise = a' : partsAfter x a'
   where (_,a) = break (== x) ls
         a' = tail a


chooseRule :: (Ord a, Show a)
           => Symbol a   -- ^ LHS non-terminal symbol A to derive from
           -> [Symbol a] -- ^ next input terminal symbols
           -> RuleMap a  -- ^ production rule map
           -> WithLog [Symbol a] -- ^ correct RHS of A to derive to
chooseRule EOF _ _ = throwError "You called chooseRule with EOF as the LHS."
chooseRule lhs ins rules = do
   chooseRuleLog $ printf "called:\n lhs: %s\n ins: %s\n rules: %s" (show lhs) (show ins) (show rules)
   x <- lookupRHSLs lhs rules
   chooseRuleR lhs 1 x ins rules


chooseRuleR :: (Ord a, Show a)
            => Symbol a     -- ^ lhs
            -> Int          -- ^ current n-th look ahead
            -> [[Symbol a]] -- ^ current candidate RHS that have the same 0-n lookahead symbols
            -> [Symbol a]   -- ^ next input terminal symbols
            -> RuleMap a    -- ^ production rule map
            -> WithLog [Symbol a]   -- ^ correct candidate RHS to derive to
chooseRuleR _ _ [candidate] _ _ = return candidate
chooseRuleR lhs n candidates (i:is) rules = do
   -- log
   chooseRuleLog $ printf
      "rhs candidates: %s\n next input: %s\n search for rhs whose first symbol is %s"
      (show candidates) (show i) (show i)
   -- code
   candidates' <- filterM (\x -> elem i <$> nthTsR x n rules) candidates
   -- log
   chooseRuleLog $ printf "Found: %s" (show candidates')
   chooseRuleLog $ "Compute the follower set for lhs: " ++ show lhs
   -- code
   fs <- follow lhs rules
   -- log
   chooseRuleLog $ "Found: " ++ show fs
   -- code
   candidates'' <- if i `elem` fs then do
                        chooseRuleLog $ printf "%s is in the follower set. Compute all nullable rhs." (show i)
                        nc <- filterM (`nullableR` rules) candidates
                        chooseRuleLog $ "Found: " ++ show nc
                        return nc
                   else return []
   let candidates''' = candidates' ++ candidates''
   when (null candidates''') (throwError $ "chooseRuleR: No production rule can be applied. Next input: " ++ show i++ "\nCandidates: " ++ show candidates)
   chooseRuleR lhs (n+1) candidates''' is rules
chooseRuleR _ n candidates [] rules = throwError $ "chooseRuleR: More than one production rule can be applied at EOF. The syntax is ambigous."
                                             ++ "\nCandidates: " ++ show candidates


lookupRHSLs :: (Ord a, Show a) => Symbol a -> RuleMap a -> WithLog [[Symbol a]]
lookupRHSLs a rules =
   case M.lookup a rules of
      Nothing -> throwError $ "LHS '"++show a++"' does not exist in "++show rules
      Just x  -> return x
