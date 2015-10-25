{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser.ProductionRule.Internal.Manager where

import Parser.ProductionRule.Internal.Maker
import Control.Monad.Writer (Writer(..), runWriter, tell, MonadWriter(..))
import Parser.ProductionRule.Internal
import Data.Set as S (Set)
import Data.Map as M (Map, fromList)
import qualified Data.Set as S (fromList)
import Control.Monad (mzero)

newtype Manager' a x = Manager {
      unManager :: Writer [Maker a] x
   } deriving (Functor, Applicative, Monad, MonadWriter [Maker a])

type Manager a = Manager' a ()

getMakers :: Manager a -> [Maker a]
getMakers m = snd $ runWriter $ unManager m


addMakers :: [Maker a] -> Manager a -> Manager a
addMakers ls m = m >> tell ls


empty :: Manager a
empty = Manager $ tell []


singleton :: Maker a -> Manager a
singleton a = addMakers [a] empty


getRules :: (Ord a) => Manager a -> Set a -> [Rule a]
getRules a nts = do
   maker <- getMakers a
   let (lhs, rhsLs) = unMaker maker
   return $ rule lhs [ reverse $ map (`setSym` nts) rhs | rhs <- rhsLs ]


getNTs :: (Ord a) => Manager a -> Set a
getNTs a = S.fromList $ do
   make <- getMakers a
   case unMaker make of
      (NT x, _) -> return x
      _ -> mzero


-- | Collect the defined syntax and produces a list of production rules.
rules :: (Ord a) => Manager a -> [Rule a]
rules a = getRules a $ getNTs a


-- | Collect the defined syntax and produces a map of production rules.
--
--  This is equivalent to `Set.fromList . rules`
ruleMap :: (Ord a) => Manager a -> RuleMap a
ruleMap = M.fromList . rules


instance FromMaker Manager' where
   fromMaker = singleton

