module Parser241.Lexer.Internal where

import Data.Foldable (asum)
import Text.Regex (matchRegexAll, mkRegex)
import Data.Maybe (mapMaybe)
import Data.List (maximumBy)
import Syntax.ListWriter as Syn (ListM, element, toList)
import Control.Arrow (first)
import Parser241.Parser

-- | A @TokenReader@ function takes @/tokenizedInput/@ and @/currentInput/@
--
-- If the input match the user-defined symbol, the Matcher should return
--
-- @Just (/tokenizedInput/, /matchedInput/, /restInput/, /userSymbol/)@, otherwise @Nothing@
type TokenReader a = [Token a] -> String -> Maybe ([Token a], String, String, a)

type Token a = (Symbol a, String)
type RegexExp = String

tokens :: ListM (TokenReader a) -> [TokenReader a]
tokens = Syn.toList


runLexer :: (Show a) => [TokenReader a] -> String -> [Token a]
runLexer tReaders ins = reverse $ ((EOF,"") :) $ runLexerAcc tReaders ins []


runLexerAcc :: (Show a) => [TokenReader a] -> String -> [Token a] -> [Token a]
runLexerAcc tReaders "" tokenized = tokenized
runLexerAcc tReaders ins tokenized
   | null matches = error $ "Unrecognized token: " ++ ins
   | otherwise = runLexerAcc tReaders restIns $ (T sym, str) : tokenized
   where
      matches = mapMaybe (\f -> f tokenized ins) tReaders
      longestMatch (_,m,_,_) (_,m',_,_) = compare (length m) (length m')
      (_, str, restIns, sym) = maximumBy longestMatch matches


(=:) :: RegexExp -> a -> ListM (TokenReader a)
regex =: sym = tokenDef sym $ matchRegex regex


-- | A @Matcher@ function takes @/tokenizedInput/@ and @/matchedInput/@
--
-- If the input match the user-defined symbol, the Matcher should return
--
-- @Just (/tokenizedInput/, /matchedInput/, /restInput/)@, otherwise @Nothing@
type Matcher a = [Token a] -> String -> Maybe ([Token a], String, String)

tokenDef :: a -> Matcher a -> ListM (TokenReader a)
tokenDef sym modifier = Syn.element $ \tokenized input -> do
   (b,m,a) <- modifier tokenized input
   return (b,m,a,sym)



matchRegex :: RegexExp -> Matcher a
matchRegex regex tokenized input = do
   (_, matched, after, _) <- matchRegexAll (mkRegex $ "^" ++ regex) input
   return (tokenized, matched, after)


