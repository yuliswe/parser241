module Parser.ProductRule (module X) where

import Parser.ProductRule.Internal as X (
        ProductRule(..)
   )
import Parser.ProductRule.Interface.Manager as X (
        productRules
   )
import Parser.ProductRule.Interface.Maker as X (
        (-->)
      , (--->)
      , (|>)
      , (|/)
      , (&)
   )

