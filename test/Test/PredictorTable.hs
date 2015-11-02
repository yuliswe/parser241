module Test.PredictorTable where

import Test.Hspec
import Parser241.Parser.PredictorTable.Internal
import Parser241.Parser.ProductionRule
import Data.Set as S (Set(..), fromList, difference, toList)
import Data.Map as M (fromList, lookup, empty)
import Control.Arrow (second)
import Data.Maybe (fromJust)
import Control.Monad.State (State, runState)
import Control.Monad (replicateM_)
import Parser241.Debug

data MySym = A | B | C | D | E | F | G | H | I | J | K deriving (Eq, Ord, Show)

data SymOp = Arith | Add' | Int' | TFac deriving (Show, Eq, Ord)

tableA :: RuleMap MySym
tableA = ruleMap $ do {
      Start ---> A;
      A --> B
   }

tableC :: RuleMap SymOp
tableC = ruleMap $ do {
      Start ---> Arith
   ;   Arith --> Int' & TFac

   ;       TFac --> Add' & Int' & TFac
                 |/ Null

   }

test :: IO ()
test = hspec $
   describe "PredictorTable" $ do

      specify "partsAfter" $ do
         partsAfter (T Add') [] `shouldBe` []
         partsAfter (T Add') [NT Arith] `shouldBe` []
         partsAfter (T Add') [NT Arith, T Add', NT Arith, T Add', NT Arith] `shouldBe` [[NT Arith, T Add', NT Arith], [NT Arith]]

      specify "followR" $ do
         fst (runWithLog $ followR (NT A) Start [EOF] tableA) `shouldBe` Right [EOF]
         fst (runWithLog $ followR (T B) (NT A) [] tableA) `shouldBe` Right [EOF]
         fst (runWithLog $ followR (NT A) Start [NT A, EOF] tableA) `shouldBe` Right [T B]

      specify "follow" $ do
         fst (runWithLog $ follow (NT A) tableA) `shouldBe` Right [EOF]
         fst (runWithLog $ follow (NT Arith) tableC) `shouldBe` Right [EOF]
         fst (runWithLog $ follow (NT TFac) tableC) `shouldBe` Right [EOF]

      specify "nullable" $
         fst (runWithLog $ nullableR [Null] tableC) `shouldBe` Right True


      specify "nthTs" $ do
         -- print $ chooseRule (NT Arith) [T Int',T Add',T Int',EOF] tableC
         -- print $ chooseRule (NT Arith) [T Int',EOF] tableC
         mapM_ print $ snd $ runWithLog $ chooseRule (NT TFac) [EOF] tableC
         fst (runWithLog $ chooseRule (NT TFac) [EOF] tableC) `shouldBe` Right [Null]
