{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser.ProductionRule.Internal.Maker where

import Parser.ProductionRule.Internal
import Data.Set (Set, singleton)
import Control.Monad.Reader (Reader(..), runReader, MonadReader(..), reader)


newtype Maker' a x = Maker {
      unMaker :: (Symbol a, [[Symbol a]])
   } deriving (Functor)

type Maker a = Maker' a ()


maker :: (Symbol a, [[Symbol a]]) -> Maker a
maker = Maker


-- | Use `--->` iff the left side is the `Start` symbol and the first symbol on the right side is an user-defined symbol.
--
--   Only one symbol is allowed on the left hand side.
--
-- > table :: [Rule MySym]
-- > table = productRules $ do
-- >    Start ---> A & B ...
-- >            ...
--
(--->) :: FromMaker m => (Ord a) => Symbol a -> a -> m a ()
lhs ---> rhs = fromMaker $ maker (lhs, [[UD rhs]])


-- | Use `-->` iff both the left side and the first symbol on the right side are user-defined symbols.
--
-- | Only one symbol is allowed on the left hand side.
--
-- | Use `&` to concatenate two user-defined symbols.
--
-- > table :: [Rule MySym]
-- > table = productRules $ do
-- >    Start ---> ...
-- >            ...
-- >       ; A --> C'
-- >            |/ Null
-- >            ...
--
(-->) :: FromMaker m => (Ord a) => a -> a -> m a ()
lhs --> rhs = fromMaker $ NT lhs ---> rhs


-- | Use `&` to concatenate two symbols.
--
-- > table :: [Rule MySym]
-- > table = productRules $ do
-- >    Start >>> Null & C'
-- >           |> ...
--
(&) :: FromMaker m => Maker a -> a -> m a ()
a & b = fromMaker $ maker (lhs, (UD b:r):rhs)
   where
      (lhs,r:rhs) = unMaker a


-- | Use `|>` to represent "or" when the left hand side can produce two different expressions,
-- and the right side is a user-defined type.
--
-- > table :: [Rule MySym]
-- > table = productRules $ do
-- >    Start ---> ...
-- >            ...
-- >            |> C'
--
(|>) :: FromMaker m => Maker a -> a -> m a ()
m |> a = fromMaker $ maker (lhs, [UD a]:rhs)
   where
      (lhs,rhs) = unMaker m


-- | Use `|/` iff the right hand side is the `Null` symbol.
--
-- > table :: [Rule MySym]
-- > table = productRules $ do
-- >    Start ---> C'
-- >            |/ Null
-- >            |> ...
--
(|/) :: FromMaker m => Maker a -> Symbol a -> m a ()
m |/ Null = fromMaker $ maker (lhs, [Null]:rhs)
   where
      (lhs,rhs) = unMaker m
_ |/ _ = error "(|/) can only be used in |/ Null"


-- | Use `>>>` iff the left hand side is `Start` and the first symbol on the right side is `Null`.
--
-- > table :: [Rule MySym]
-- > table = productRules $ do
-- >    Start >>> Null
-- >           |> C'
-- >           ...
--
(>>>) :: FromMaker m => Symbol a -> Symbol a -> m a ()
Start >>> Null = fromMaker $ maker (Start, [[Null]])
_ >>> _ = error "(>>>) can only be used in Start >>> Null ..."


class FromMaker m where
   fromMaker :: Maker a -> m a ()


instance FromMaker Maker' where
   fromMaker = id
