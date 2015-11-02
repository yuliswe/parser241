import Test.Hspec

import Test.ProductionRule
import Test.PredictorTable
import Test.Lexer
import Test.Parser

main :: IO ()
main = do
   Test.ProductionRule.test
   Test.PredictorTable.test
   Test.Lexer.test
   Test.Parser.test
