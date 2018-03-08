{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import Control.Monad.Trans
import Unbound.Generics.LocallyNameless
import System.Console.Haskeline
import System.Exit
import MuPRL.Parser.Parser
import MuPRL.Syntax
import MuPRL.PrettyPrint
import MuPRL.Rules

type Refinement = ReaderT [(Var, Term)] (FreshMT (ExceptT RuleError IO))

runRefinement :: Refinement a -> IO a
runRefinement r = do
    r <- runExceptT $ runFreshMT $ runReaderT r []
    case r of
        Right x -> return x
        Left err -> die $ show err

hoistErr :: Show e => Either e a -> Refinement a
hoistErr (Right val) = return val
hoistErr (Left err) = liftIO $ die $ show err
--   abort

getRule :: Goal -> Refinement (Rule Refinement)
getRule g = do
    liftIO $ putStrLn $ pp g
    str <- liftIO $ getLine
    hoistErr $ runParser rule str

main :: IO ()
main = runRefinement $ do
    str <- liftIO $ getLine
    t <- hoistErr $ runParser term str
    refine t getRule

