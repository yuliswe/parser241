{-# LANGUAGE FlexibleInstances #-}

module Parser241.Debug.Internal where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Writer (WriterT, runWriterT, tell, writer, listen)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)


data Log = Parser String
         | ChooseRule String
         | Lexer String

type WithLog a = ExceptT String (WriterT [Log] Identity) a

parserLog :: String -> WithLog ()
parserLog a = tell [Parser a]

chooseRuleLog :: String -> WithLog ()
chooseRuleLog a = tell [ChooseRule a]

lexerLog :: String -> WithLog ()
lexerLog a = tell [Lexer a]

runWithLog :: WithLog a -> (Either String a, [Log])
runWithLog a = runIdentity $ runWriterT $ runExceptT a

toString :: Log -> String
toString (Parser m) = "Parser: " ++ m
toString (ChooseRule m) = "Parser.ChooseRule: " ++ m
toString (Lexer m) = "Lexer: " ++ m

instance Show Log where
   show = toString
