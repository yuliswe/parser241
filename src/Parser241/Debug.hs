module Parser241.Debug (
        WithLog
      , parserLog
      , lexerLog
      , getLog
      , throwError
      , catchError
      , traceShowId
   ) where

import Parser241.Debug.Internal
import Control.Monad.Except
import Debug.Trace
