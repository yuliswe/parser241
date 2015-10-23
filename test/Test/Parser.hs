module Test.Parser where

import Test.Hspec
import Parser.ProductRule
import Parser.ProductRule.Internal (Symbol(..))
import Data.Set as S (fromList)
import Data.Map as M (fromList)


data TestNT = A0 | B0 | C0 deriving (Eq, Show, Ord)


tableA :: [ProductRule TestNT]
tableA = productRules $ do
   Start ---> A0 & B0 & C0
      |> A0 & B0
      |/ Null
   A0 --> B0 & C0 & A0
      |> A0 & C0
      |/ Null


data MySym = A
           | B
           | C'
         deriving (Eq, Show, Ord)

tableB :: [ProductRule MySym]
tableB = productRules $ do

   Start ---> A & C' & B  -- AC'B concatenation
           |> A
           |> C'

      ; A --> B
           |/ Null
           |> A & C'

      ; B --> C'

tableC :: [ProductRule MySym]
tableC = productRules $
   Start >>> Null & C'
          |> C'


test :: IO ()
test = hspec $ do

   describe "sampleTableA" $
      it "productRules" $
         S.fromList tableA `shouldBe` S.fromList [
              (Start, [NT A0, T B0, T C0])
            , (Start, [NT A0, T B0])
            , (Start, [Null])
            , (NT A0, [T B0, T C0, NT A0])
            , (NT A0, [NT A0, T C0])
            , (NT A0, [Null])
         ]

   describe "sampleTableB" $
      it "productRules" $
         S.fromList tableB `shouldBe` S.fromList [
              (Start, [NT A, T C', NT B])
            , (Start, [NT A])
            , (Start, [T C'])
            , (NT A, [NT B])
            , (NT A, [NT A, T C'])
            , (NT A, [Null])
            , (NT B, [T C'])
         ]

   describe "sampleTableC" $
      it "productRules" $
         S.fromList tableC `shouldBe` S.fromList [(Start, [Null, T C']), (Start, [T C'])]

