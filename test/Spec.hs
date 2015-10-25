import Test.Hspec

import Test.ProductionRule
import Test.PredictorTable

main :: IO ()
main = do
   Test.ProductionRule.test
   Test.PredictorTable.test
