import Test.Hspec

import Test.PredictorTable
import Test.Parser

main :: IO ()
main = do
   Test.PredictorTable.test
   Test.Parser.test
