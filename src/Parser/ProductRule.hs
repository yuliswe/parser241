-- | Originally desgined as a cheating tool for cs241, a course offered in the University of Waterloo.
--
-- This module contains everything you need to define an augmented grammar.
--
-- This module is a monadic interface to define an augmented grammar.
-- The function `productRules` defined in this package takes in an abstract syntax tree representation,
-- and produces a production rule table, in which non-terminal and terminal symbols are labeled,
-- and can be further used by you parser project.
--
--
-- For example, given a user-defined symbols,
--
-- > data MySym = A
-- >            | B
-- >            | C'
-- >          deriving (Eq, Show, Ord)
--
-- where A B are non-terminal symbols, 'C 'D are terminal symbols, we can define a production rule table:
--
-- > table :: [ProductRule MySym]
-- > table = productRules $ do
-- >
-- >    Start ---> A & C' & B  -- AC'B concatenation
-- >            |> A
-- >            |> C'
-- >
-- >       ; A --> B           -- You might want to use ";" to clarify the indentation in a `do` block.
-- >            |> A & C'
-- >            |/ Null
-- >
-- >       ; B --> C'
-- >
--
-- This will produce:
--
-- >>> print $ table
-- >
-- >  [    (Start, [NT A, T C', NT B])
-- >     , (Start, [NT A])
-- >     , (Start, [T C'])
-- >     , (NT A, [NT B])
-- >     , (NT A, [NT A, T C'])
-- >     , (NT A, [Null])
-- >     , (NT B, [T C'])
-- >  ]
-- >
--
-- where `NT` represents non-terminal type, and `T` represents terminal type.
--
-- This package does not parse the input in any way. It just simplifies the way you can define the grammar.

module Parser.ProductRule (
      Symbol(Start, Null, NT, T)
    , ProductRule
    , productRules
    -- * Production Rule Construction
    , (--->)
    , (-->)
    , (|>)
    , (&)
    , (|/)
    , (>>>)
  ) where

import Parser.ProductRule.Internal as X (
        ProductRule(..)
      , Symbol(..)
   )
import Parser.ProductRule.Internal.Manager as X (
        productRules
   )
import Parser.ProductRule.Internal.Maker
import Parser.ProductRule.Internal.Maker as X (
        (-->)
      , (--->)
      , (|>)
      , (|/)
      , (&)
      , (>>>)
   )

