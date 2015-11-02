module Test.Parser where

import Test.Hspec
import Parser241.Lexer
import Parser241.Parser.LL
import Parser241.Parser
import Parser241.Debug
import Data.Tree (Tree(..), drawTree)

import Control.Monad.Except (runExceptT)

data SymOp = Arith | Add' | Int' | Space' | TFac deriving (Show, Eq, Ord)


removeSpaces :: [Token SymOp] -> [Token SymOp]
removeSpaces = filter (not . isSpace)


isSpace :: Token SymOp -> Bool
isSpace (T Space',_) = True
isSpace _ = False


rulesC :: RuleMap SymOp
rulesC = ruleMap $ do {
         Start ---> Arith
      ;   Arith --> Int' & TFac
      ; TFac --> Add' & Int' & TFac
              |/ Null
   }

tokensC :: [TokenReader SymOp]
tokensC = tokens $ do
   "[0-9]" =: Int'
   "\\+" =: Add'
   "\\s"   =: Space'


test :: IO ()
test = hspec $
   describe "Parser" $
      specify "test" $ do
         let (r,l) = runWithLog $ parse rulesC $ removeSpaces $ runLexer tokensC "1+2+3"
         r `shouldBe` Right ([0,3,3,2,1],[(NT TFac,""),(T Int',"3"),(T Add',"+"),(NT TFac,""),(T Int',"2"),(T Add',"+"),(NT TFac,""),(T Int',"1"),(NT Arith,""),(Start,"")])
