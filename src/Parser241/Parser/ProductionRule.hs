-----------------------------------------------------------------------------
-- |
-- Module      :  Parser241.Parser.ProductionRule
-- Copyright   :  See LICENSE
--
-- Maintainer  :  ylilarry@gmail.com
-- Stability   :  Experimental
-- Portability :  Non-portable (GHC extensions)
--
-- = Introduction
--
-- __This module contains everything you need to define an augmented grammar.__
--
-- This module is a monadic interface to define an augmented grammar.
-- The function `rules` defined in this package takes in an abstract syntax tree representation,
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
-- > table :: [Rule MySym]
-- > table = rules $ do
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
-- >  [    (Start, [ [NT A, T C', NT B] , [NT A]       , [T C'] ])
-- >     , (NT A,  [ [NT B]             , [NT A, T C'] , [Null] ])
-- >     , (NT B,  [ [T C']                                     ])
-- >  ]
-- >
--
-- where `NT` represents non-terminal type, and `T` represents terminal type.
--
-- __This package does not parse the input in any way. It just simplifies the way you can define the grammar.__
--
-----------------------------------------------------------------------------

module Parser241.Parser.ProductionRule (
      Symbol(Start, Null, NT, T, EOF)
    , Rule
    , RuleMap
    , rules
    , ruleMap
    -- * Production Rule Construction
    , (--->)
    , (-->)
    , (|>)
    , (&)
    , (|/)
    , (>>>)
  ) where

import Parser241.Parser.ProductionRule.Internal as X (
        Rule(..)
      , Symbol(..)
      , RuleMap(..)
   )
import Parser241.Parser.ProductionRule.Internal.Manager as X (
        rules
      , ruleMap
   )
import Parser241.Parser.ProductionRule.Internal.Maker
import Parser241.Parser.ProductionRule.Internal.Maker as X (
        (-->)
      , (--->)
      , (|>)
      , (|/)
      , (&)
      , (>>>)
   )

