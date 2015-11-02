module Parser241.Parser.LL.Internal where

import Parser241.Parser.ProductionRule
import Parser241.Parser.PredictorTable
import Parser241.Lexer
import Parser241.Debug

import Control.Arrow (first, second)
import Text.Printf (printf)
import Control.Monad (unless)
import Data.Tree (Tree(..))

runParser :: (Show a, Eq a, Ord a) =>
             RuleMap a                                -- ^ production rules
          -> [Token a]                                -- ^ input tokens
          -> WithLog (Tree (Token a))   -- ^ ([# of popped symbols], [popped symbol])
runParser rules input = do
   (ns, ls) <- _parse rules [Start] input [] []
   return $ toTree ns [ Node l [] | l <- ls]


parse :: (Show a, Eq a, Ord a) =>
         RuleMap a                                -- ^ production rules
      -> [Token a]                                -- ^ input tokens
      -> WithLog ([Int], [Token a])   -- ^ ([# of popped symbols], [popped symbol])
parse rules input = _parse rules [Start] input [] []


_parse :: (Show a, Eq a, Ord a) =>
          RuleMap a                              -- ^ production rules
       -> [Symbol a]                             -- ^ working stack
       -> [Token a]                              -- ^ input tokens
       -> [Int]
       -> [Token a]                              -- ^ ([# of popped symbols], [popped symbol])
       -> WithLog ([Int], [Token a])
_parse _ (EOF:_) ((EOF,""):_) nums syms = return (nums, syms)
_parse _ (EOF:_) tokens _ _ = throwError $ "parse: leftover symbol at: " ++ show tokens
_parse _ stack [] nums syms = throwError $ "parse: expecting more symbols at EOF."
                                      ++ "\nParsed: " ++ show syms
                                      ++ "\nParsed: " ++ show stack
_parse rules w@(T s:ss) tokens@(token@(t,str):ts) nums syms = do
   logParams rules w tokens nums syms
   unless (T s == t) (throwError $ "parse: unexpected symbol at: " ++ show token)
   _parse rules ss ts nums (token:syms)
_parse rules (Null:ss) tokens nums syms = _parse rules ss tokens nums syms
_parse rules w@(s:ss) tokens nums syms = do
   logParams rules w tokens nums syms
   parserLog $ printf "Try to choose a rule deriving from %s" (show s)
   rhs <- chooseRule s (fst <$> tokens) rules
   parserLog $ printf "Use rule %s --> %s" (show s) (show rhs)
   let len = length rhs - length (filter isNP rhs)
   _parse rules (rhs ++ ss) tokens (len:nums) ((s, ""):syms)


logParams :: (Show a) => RuleMap a -> [Symbol a] -> [Token a] -> [Int] -> [Token a] -> WithLog ()
logParams rules working ins nums syms = parserLog $
   printf "Called: _parse \n rules: %s\n working: %s\n ins: %s\n nums: %s\n syms: %s"
      (show rules) (show working) (show ins) (show nums) (show syms)


toTree :: [Int] -> [Tree (Token a)] -> Tree (Token a)
toTree _ [] = error "toTree called on [] list."
toTree [] trees = head trees
toTree (n:ns) trees
   | null rst = head trees
   | otherwise = toTree ns ((head rst){subForest = ntrees} : tail rst)
   where (ntrees,rst) = splitAt n trees


