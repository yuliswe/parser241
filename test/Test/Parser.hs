module Test.Parser where

import Test.Hspec
import Parser.ProductRule
import Parser.ProductRule.Internal (Symbol(..))
import Data.Set as S (fromList)
import Data.Map as M (fromList)


data TestNT = A | B | C
            | A' | B' | C' deriving (Eq, Show, Ord)


tableA :: [ProductRule TestNT]
tableA = productRules $ do
   Start ---> A & B & C
      |> A & B
      |/ Null
   A --> B & C & A
      |> A & C
      |/ Null


test :: IO ()
test = hspec $
   describe "sampleTableA" $
      it "productRules" $
         S.fromList tableA `shouldBe` S.fromList [
              (Start, [NT A, T B, T C])
            , (Start, [NT A, T B])
            , (Start, [Null])
            , (NT A, [T B, T C, NT A])
            , (NT A, [NT A, T C])
            , (NT A, [Null])
         ]

