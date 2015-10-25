{-# LANGUAGE ScopedTypeVariables #-}

module Parser.PredictorTable where

import Parser.ProductionRule (Rule, Symbol(..), RuleMap)
import Parser.ProductionRule.Internal (unsetSym, isT)
import Data.Graph (Graph, graphFromEdges, Vertex, reachable)
import Control.Arrow (second)
import Data.Set as S (Set, difference, fromList, toList)
import Data.Maybe (catMaybes, mapMaybe, fromJust)
import Data.Map.Lazy as M (Map, fromList, lookup)


type Vtx2Node a = Vertex -> ((), Symbol a, [Symbol a])
type Node2Vtx a = Symbol a -> Maybe Vertex
type Sym2Vtx a = Symbol a -> Vertex
type Vtx2Sym a = Vertex -> Symbol a


snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x


-- | `toGraph` consumes a list of production rules and produces a graph
toGraph :: (Ord a) => [Rule a] -> (Graph, Vtx2Node a, Node2Vtx a)
toGraph rules = mkGraph (map (immSymsRhs `second`) rules) $ S.toList ts
   where
      (_, ts) = getSyms rules


-- | `mkGraph` consumes [(lhs, [fst-symbol])] and [terminals] and produces a graph
mkGraph :: (Ord a) => [(Symbol a, [Symbol a])] -> [Symbol a] -> (Graph, Vtx2Node a, Node2Vtx a)
mkGraph al ts = graphFromEdges $
   [ ((), lhs, fstSyms) | (lhs, fstSyms) <- al ]
   ++ [ ((), t, []) | t <- ts ]


-- | `immSymsRhs` cosumes a list of rhs and produces the a list of first rhs syms
immSymsRhs :: [[Symbol a]] -> [Symbol a]
immSymsRhs = concatMap (take 1)


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a


allSyms :: (Ord a) => [Rule a] -> Set (Symbol a)
allSyms a = S.fromList $ do
   (lhs, rhsLs) <- a
   lhs : concat rhsLs


getNTs :: (Ord a) => [Rule a] -> [Symbol a]
getNTs = map fst


getSyms :: (Ord a) => [Rule a] -> (Set (Symbol a), Set (Symbol a))
getSyms ls = let nts = S.fromList $ getNTs ls in (nts, allSyms ls `difference` nts)


fstSyms :: Symbol a   -- ^ from this symbol
        -> Graph      -- ^ first symbol graph
        -> Sym2Vtx a
        -> Vtx2Sym a
        -> [Symbol a] -- ^ all first symbols
fstSyms from graph sym2vtx vtx2sym = map vtx2sym $ reachable graph (sym2vtx from)


fstTs :: (Ord a) => Symbol a -> [Rule a] -> [Symbol a]
fstTs a rules = filter isT $ fstSyms a graph sym2vtx vtx2sym
   where
      (graph, v2n, n2v) = toGraph rules
      vtx2sym = snd3 . v2n
      sym2vtx = fromJust . n2v


infFstTs :: (Ord a) => Symbol a -> [Rule a] -> [[Symbol a]]
infFstTs a rules = fstTs a rules : infFstTs a (dropFstRHS rules)


nthTs :: (Ord a)
      => Symbol a
      -> Int        -- ^ `n`-th symbols
      -> RuleMap a  -- ^ rules
      -> [Symbol a] -- ^ n-th symbols
nthTs (T a) 1 _ = [T a]
nthTs (T _) _ _ = []
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
nthTsR (r:rs) n rules = nthTs r n rules ++ nthTsR rs (n-1) rules


dropFstRHS :: [Rule a] -> [Rule a]
dropFstRHS = map (drop 1 `second`)


-- | predictor table that contains infinite look ahead symbols
predictorMap :: (Ord a) => [Rule a] -> RuleMap a
predictorMap rules = M.fromList $ predictorTable rules


predictorTable :: (Ord a) => [Rule a] -> [(Symbol a, [[Symbol a]])]
predictorTable rules = [ (lhs, infFstTs lhs rules) | (lhs,_) <- rules ]


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
      (filter (\x -> i `elem` nthTsR x n rules) candidates)
      is
      rules
