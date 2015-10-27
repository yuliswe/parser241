module Test.PredictorTable where

import Test.Hspec
import Parser241.Parser.PredictorTable
import Parser241.Parser.ProductionRule
import Data.Set as S (Set(..), fromList, difference, toList)
import Data.Map as M (fromList, lookup, empty)
import Control.Arrow (second)
import Data.Maybe (fromJust)
import Control.Monad.State (State, runState)
import Control.Monad (replicateM_)

data MySym = A | B | C | D | E | F | G | H | I | J | K deriving (Eq, Ord, Show)

table0 :: [Rule MySym]
table0 = rules $ do
   Start ---> A & B & C & D & E & F & G & H & I & J & K
           |> A & C & D & E & F & G & H & I & J & K
           |> A & D & E & F & G & H & I & J & K
           |> A & E & F & G & H & I & J & K
           |> A & F & G & H & I & J & K
           |> A & G & H & I & J & K
           |> A & H & I & J & K
           |> A & I & J & K
           |> A & J & K
           |> A & K


tableA :: [Rule MySym]
tableA = rules $ do
   Start ---> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & A
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B
           |> A & A & A & A & A & A & A & A & A & A & A & A & A & A & A & B

test :: IO ()
-- test = hspec $ do
--    specify "PredictorTable" $ do
--       describe "toGraph" $ do


runWithCache x = runState x M.empty

test = hspec $ do
   specify "nthTs" $ do
      print $ chooseRule Start [A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A] $ M.fromList tableA
