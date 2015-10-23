{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser.ProductRule.Interface.Maker where
import Parser.ProductRule.Internal
import Data.Set (Set, singleton)
import Control.Monad.Reader (Reader(..), runReader, MonadReader(..), reader)


-- | (makerLhs, rhs)
newtype Maker' a x = Maker {
      unMaker :: (Symbol a, [[Symbol a]])
   } deriving (Functor)

type Maker a = Maker' a ()


maker :: (Symbol a, [[Symbol a]]) -> Maker a
maker = Maker


(--->) :: (Ord a) => Symbol a -> a -> Maker a
lhs ---> rhs = maker (lhs, [[UD rhs]])

(-->) :: (Ord a) => a -> a -> Maker a
lhs --> rhs = NT lhs ---> rhs


(&) :: FromMaker m => Maker a -> a -> m a ()
a & b = fromMaker $ maker (lhs, (UD b:r):rhs)
   where
      (lhs,r:rhs) = unMaker a


(|>) :: FromMaker m => Maker a -> a -> m a ()
m |> a = fromMaker $ maker (lhs, [UD a]:rhs)
   where
      (lhs,rhs) = unMaker m


(|/) :: FromMaker m => Maker a -> Symbol a -> m a ()
m |/ Null = fromMaker $ maker (lhs, [Null]:rhs)
   where
      (lhs,rhs) = unMaker m

class FromMaker m where
   fromMaker :: Maker a -> m a ()


instance FromMaker Maker' where
   fromMaker = id
