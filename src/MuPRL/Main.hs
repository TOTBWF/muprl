{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Control.Applicative
-- import Control.Monad.Reader
-- import Control.Monad.Except
-- import Control.Monad
-- import Control.Monad.Trans
-- import Unbound.Generics.LocallyNameless
-- import System.Console.ANSI
-- import Data.List (isPrefixOf)
-- import qualified Data.Sequence as Seq

import MuPRL.Parser.Term (term)
import MuPRL.Parser.Stack
import MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.ProofState
import MuPRL.Refine.Judgements

import MuPRL.Repl.MonadRepl
import MuPRL.Repl.Repl


loop :: Repl ()
loop = do
    input <- getInputLine "μPRL> "
    case input of
        Just i -> case runParser term i of
            Left err -> printError err >> loop
            Right t -> do
                extract <- runRefine $ refine (Tl.empty |- t)
                outputStr "Extract: " >> displayLn extract
                loop
        Nothing -> outputStrLn "Goodbye"


main :: IO ()
main = runReplT loop