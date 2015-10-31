module Parser241.Parser.LL.Internal where

import Parser241.Parser.ProductionRule
import Parser241.Parser.PredictorTable
import Parser241.Lexer
import Parser241.Debug

import Control.Arrow (first, second)
import Text.Printf (printf)

runParser :: (Show a, Eq a, Ord a) =>
             RuleMap a                                -- ^ production rules
          -> [Token a]                                -- ^ input tokens
          -> WithLog ([Int], [Token a])   -- ^ ([# of popped symbols], [popped symbol])
runParser rules = _parse rules [Start] [] []

_parse :: (Show a, Eq a, Ord a) =>
          RuleMap a                              -- ^ production rules
       -> [Symbol a]                             -- ^ working stack
       -> [Token a]                              -- ^ input tokens
       -> [Int]
       -> [Token a]                              -- ^ ([# of popped symbols], [popped symbol])
       -> WithLog ([Int], [Token a])
_parse _ [EOF] [(EOF,"")] nums syms = return (nums, syms)
_parse _ [EOF] tokens _ _ = error $ "parse: leftover symbol at: " ++ show tokens
_parse _ stack [(EOF,"")] nums syms = error $ "parse: expecting more symbols at EOF."
                                      ++ "\nParsed: " ++ show syms
                                      ++ "\nParsed: " ++ show stack
_parse rules (T s:ss) (token@(t,str):ts) nums syms
   | T s == t  = _parse rules ss ts nums (token:syms)
   | otherwise = error $ "parse: unexpected symbol at: " ++ show token
_parse rules w@(s:ss) tokens nums syms = do
   logParams rules w tokens nums syms
   rhs <- chooseRule s (fst <$> tokens) rules
   _parse rules (rhs ++ ss) tokens (length rhs:nums) ((s, ""):syms)


logParams :: (Show a) => RuleMap a -> [Symbol a] -> [Token a] -> [Int] -> [Token a] -> WithLog ()
logParams rules working ins nums syms = parserLog $
   printf "Called: _parse \n rules: %s\n working: %s\n ins: %s\n nums: %s\n syms: %s\n"
      (show rules) (show working) (show ins) (show nums) (show syms)

