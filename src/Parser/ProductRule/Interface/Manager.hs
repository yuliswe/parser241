{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser.ProductRule.Interface.Manager where

import Parser.ProductRule.Interface.Maker
import Control.Monad.Writer (Writer(..), runWriter, tell, MonadWriter(..))
import Parser.ProductRule.Internal
import Data.Set (Set)
import qualified Data.Set as S (fromList)
import Control.Monad (mzero)

-- | (lhs, nonterms -> product rule)
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


getRules :: (Ord a) => Manager a -> Set a -> [ProductRule a]
getRules a nts = do
   maker <- getMakers a
   let (lhs, rhs) = unMaker maker
   rs <- rhs
   return $ rule lhs $ reverse $ map (`setT` nts) rs


getNTs :: (Ord a) => Manager a -> Set a
getNTs a = S.fromList $ do
   make <- getMakers a
   case unMaker make of
      (NT x, _) -> return x
      _ -> mzero


productRules :: (Ord a) => Manager a -> [ProductRule a]
productRules a = getRules a $ getNTs a


instance FromMaker Manager' where
   fromMaker = singleton

