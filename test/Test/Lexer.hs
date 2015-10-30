module Test.Lexer where

import Parser241.Lexer.Internal
import Test.Hspec
import Syntax.ListWriter as Syn

data MySym = LBrace | RBrace | Type | Identifer | Space deriving (Show, Eq, Ord)

table0 :: [TokenReader MySym]
table0 = tokens $ do
   "\\{" =: LBrace
   "\\}" =: RBrace
   "\\s" =: Space
   "int" =: Type
   " "   =: Space
   "[A-z]+" =: Identifer


test :: IO ()
test = hspec $
   describe "Lexer" $ do
      specify "runLexer" $ do
         print $ runLexer table0 "int intmain"
         -- print $ runLexer "int main() {}" table0
