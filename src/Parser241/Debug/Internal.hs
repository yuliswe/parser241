{-# LANGUAGE FlexibleInstances #-}

module Parser241.Debug.Internal where

import Control.Monad.Identity (Identity)
import Control.Monad.Writer (WriterT, runWriterT, tell, writer, listen)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)


data Log = Parser String
         | Lexer String
      deriving (Show)

type WithLog a = WriterT [Log] (ExceptT String Identity) a

parserLog :: String -> WithLog ()
parserLog a = tell [Parser a]

lexerLog :: String -> WithLog ()
lexerLog a = tell [Lexer a]

getLog :: WithLog a -> WithLog [Log]
getLog a = do
   (_,w) <- listen a
   return w
