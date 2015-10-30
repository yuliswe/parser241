module Parser241.Parser.LL.Internal where

import Parser241.Parser.ProductionRule
import Parser241.Parser.PredictorTable
import Parser241.Lexer

import Control.Arrow (first, second)

runParser :: (Show a, Eq a, Ord a) =>
             RuleMap a                                -- ^ production rules
          -> [Token a]                                -- ^ input tokens
          -> ([Int], [Token a])   -- ^ ([# of popped symbols], [popped symbol])
runParser rules = _parse rules [Start]

_parse :: (Show a, Eq a, Ord a) =>
          RuleMap a                              -- ^ production rules
       -> [Symbol a]                             -- ^ working stack
       -> [Token a]                     -- ^ input tokens
       -> ([Int], [Token a]) -- ^ ([# of popped symbols], [popped symbol])
_parse _ [] []     = ([],[])
_parse _ [] tokens = error $ "parse: unexpected symbol at: " ++ show tokens
_parse _ stack  [] = error $ "parse: expecting more symbols at: " ++ show stack
_parse rules (T s:ss) (token@(t,str):ts)
   | T s == t  = (token :) `second` _parse rules ss ts
   | otherwise = error $ "parse: unexpected symbol at: " ++ show token
_parse rules (s:ss) tokens = (length rhs, (s, "")) `cons` _parse rules (rhs ++ ss) tokens
   where
      rhs = chooseRule s (fst <$> tokens) rules
      cons (a,b) (as,bs) = (a:as, b:bs)

