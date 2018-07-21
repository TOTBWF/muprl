{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.IO.Class
import Data.Foldable (traverse_)

import Options.Applicative

import MuPRL.Parser.Term (term)
import MuPRL.Parser.Vernacular (vernacular)
import MuPRL.Parser.Stack (runParser)
import MuPRL.Error
import MuPRL.PrettyPrint
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Unbound.MonadName (runNameMT)
import MuPRL.Refine.ProofState
import MuPRL.Refine.Judgement
import MuPRL.Refine.Tactic
import MuPRL.Vernacular.Syntax
import MuPRL.Vernacular.Eval

import MuPRL.Repl.MonadRepl
import MuPRL.Repl.Repl

data Options = Options
    { file :: FilePath
    }

options :: Parser Options
options = Options
    <$> argument str
        (metavar "FILENAME"
        <> help "the file to execute")

main :: IO ()
main = (\o -> runReplT $ execFile $ file o) =<< execParser opts
    where
        opts = info (options <**> helper)
            (fullDesc
            <> progDesc "check the file FILENAME"
            <> header "μPRL")
-- main = runReplT loop
-- main = runReplT $ execFile "samples/id.mprl"

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

execFile :: FilePath -> Repl ()
execFile f = do
    contents <- liftIO $ T.readFile f
    p <- hoistErr $ runParser vernacular contents
    extracts <- traverse (\v -> hoistErr =<< runNameMT (evalVernacular v)) p
    traverse_ displayLn extracts

-- execVernacular :: (Fresh m, MonadIO m) => Vernacular m -> m Term
-- execVernacular (Theorem _ goal tac) = do
--     (ProofState bnd) <- hoistErr =<< runTactic (Tl.empty |- goal) tac
--     (subgoals, extract) <- unbind bnd
--     undefined
--     if (Tl.null subgoals) 
--         then return extract
--         else 

hoistErr :: (Error e, MonadIO m) => Either e a -> m a
hoistErr (Left err) = printErr err
hoistErr (Right a) = return a

printErr :: (Error e, MonadIO m) => e -> m a
printErr err = liftIO $ die $ T.unpack $ renderError err