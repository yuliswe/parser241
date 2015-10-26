module Test.PredictorTable where

import Test.Hspec
import Parser.PredictorTable
import Parser.ProductionRule
import Data.Set as S (Set(..), fromList, difference, toList)
import Data.Map as M (fromList, lookup, empty)
import Control.Arrow (second)
import Data.Maybe (fromJust)
import Control.Monad.State (State, runState)

data MySym = A | B | C' | D' | E' deriving (Eq, Ord, Show)

table0 :: [Rule MySym]
table0 = rules $ do
   Start ---> A


tableA :: [Rule MySym]
tableA = rules $ do
   Start ---> A & E'
           |> B & E'
   A --> C' & A
   B --> D'

test :: IO ()
-- test = hspec $ do
--    specify "PredictorTable" $ do
--       describe "toGraph" $ do


runWithCache x = runState x M.empty

test = do
   print $ runWithCache $ nthTs Start 100 $ M.fromList tableA
   print $ chooseRule Start [D'] $ M.fromList tableA
