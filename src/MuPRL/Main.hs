{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import Control.Monad.Trans
import Unbound.Generics.LocallyNameless
import System.Console.Repline
import System.Console.ANSI
import Data.List (isPrefixOf)

import MuPRL.Parser.Parser
import MuPRL.Syntax
import MuPRL.PrettyPrint
import MuPRL.Rules
import MuPRL.Refinement
import MuPRL.Error

type Repl = HaskelineT IO 

exec :: String -> Repl ()
exec line = 
    case runParser term line of
        Left err -> printErr err
        Right t -> liftIO $ runRefinement $ refine t

cmd :: [(String, [String] -> Repl ())]
cmd = []

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = []

comp :: (Monad m) => WordCompleter m
comp n = do
    let cmds = ((':':) . fst) <$> cmd
    return $ filter (isPrefixOf n) cmds

completer :: CompleterStyle IO
completer = Prefix (wordCompleter comp) defaultMatcher



main :: IO ()
main = evalRepl "μPRL>" exec cmd completer (return ())
