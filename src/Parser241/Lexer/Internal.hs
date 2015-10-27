module Parser241.Lexer.Internal where

import Data.Foldable (asum)
-- import Parser241.Parser (Symbol)
import Text.Regex

import Debug.Trace

type TokenReader a = String -> Maybe (a, String, String)
type Token a = (a, String)

runLexer :: String -> [TokenReader a] -> [Token a]
runLexer "" tReaders = []
runLexer ins tReaders = (sym, str) : runLexer restIns tReaders
   where
      Just (sym, str, restIns) = asum $ map (\f -> f ins) tReaders


(=:) :: String -> a -> [TokenReader a]
regex =: cons = [tokenReader regex cons id]


tokenReader :: String                -- ^ regex
            -> a                     -- ^ User-defined symbol
            -> (String -> String)    -- ^ modify the string in a token
            -> TokenReader a
tokenReader regex sym modifier str =
   case traceShowId $ matchRegexAll (mkRegex $ "^" ++ regex) (traceShowId str) of
      Nothing -> Nothing
      Just (_,matched,after,_) -> Just (sym, modifier matched, after)
