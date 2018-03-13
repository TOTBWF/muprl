{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import Control.Monad.Trans
import Unbound.Generics.LocallyNameless
import System.Console.ANSI
import Data.List (isPrefixOf)

import MuPRL.Parser.Parser
import MuPRL.Syntax
import MuPRL.PrettyPrint
import MuPRL.Rules
import MuPRL.Refinement
import MuPRL.Repl


loop :: Repl ()
loop = do
    input <- getInputLine "μPRL>"
    case input of
        Just i -> case runParser term i of
            Left err -> printErr err >> loop
            Right t -> do
                extract <- (runRefinement $ refine t)
                outputStrLn $ pp extract
                loop
        Nothing -> outputStrLn "Goodbye"


main :: IO ()
main = runReplT loop

    --evalRepl "μPRL>" exec cmd completer (return ())
