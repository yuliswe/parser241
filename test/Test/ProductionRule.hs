module Test.ProductionRule where

import Test.Hspec
import Parser.ProductionRule
import Parser.ProductionRule.Internal (Symbol(..))
import Data.Set as S (Set(..), fromList)
import Data.Map as M (fromList)
import Control.Arrow (second)

data TestNT = A0 | B0 | C0 deriving (Eq, Show, Ord)


tableA :: [Rule TestNT]
tableA = rules $ do
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

tableB :: [Rule MySym]
tableB = rules $ do

   Start ---> A & C' & B  -- AC'B concatenation
           |> A
           |> C'

      ; A --> B
           |/ Null
           |> A & C'

      ; B --> C'

tableC :: [Rule MySym]
tableC = rules $
   Start >>> Null & C'
          |> C'


ignoreOrder :: (Ord a) => [Rule a] -> Set (Symbol a, Set [Symbol a])
ignoreOrder = S.fromList . map (S.fromList `second`)

test :: IO ()
test = hspec $ do

   describe "sampleTableA" $
      it "rules" $
         ignoreOrder tableA `shouldBe` ignoreOrder [
              (Start, [
                        [NT A0, T B0, T C0],
                        [NT A0, T B0],
                        [Null] ])
            , (NT A0, [
                        [T B0, T C0, NT A0],
                        [NT A0, T C0],
                        [Null] ])
         ]

   describe "sampleTableB" $
      it "rules" $
         ignoreOrder tableB `shouldBe` ignoreOrder [
              (Start, [ [NT A, T C', NT B],
                        [NT A],
                        [T C'] ] )
            , (NT A, [ [NT B],
                       [NT A, T C'],
                       [Null] ])
            , (NT B, [ [T C'] ])
         ]

   describe "sampleTableC" $
      it "rules" $
         ignoreOrder tableC `shouldBe` ignoreOrder [(Start, [ [Null, T C'], [T C'] ])]
