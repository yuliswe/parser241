module Test.Lexer where

import Parser241.Lexer.Internal
import Test.Hspec

data MySym = A | B | C | D deriving (Show, Eq, Ord)

table0 :: [TokenReader MySym]
table0 = do
   "a" =: A


test :: IO ()
test = hspec $
   specify "runLexer" $ do
      putStrLn "Hello World"
      putStrLn "Hello World"
      print $ runLexer "aa" table0
