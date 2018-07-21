{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Repl.Repl where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import MuPRL.Parser.Stack
import qualified MuPRL.Parser.Tactic as P

import qualified Data.Text as T
import Data.Text (Text)

import MuPRL.Core.Term
import MuPRL.Core.Unbound.MonadName

import MuPRL.Error
import MuPRL.Refine.ProofState
import MuPRL.Refine.Judgement
import MuPRL.Refine.Tactic
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))

import MuPRL.Repl.MonadRepl

type Refine = ReaderT Int (NameMT (ExceptT Text Repl))

runRefine :: Refine a -> Repl a
runRefine r = do
    r <- runExceptT $ runNameMT $ runReaderT r 0
    case r of
        Left err -> outputStrLn err >> abort
        Right a -> return a

refine :: Judgement -> Refine Extract
refine j = do
    (goals, extract) <- unbind =<< tryTactic j
    outputStr "Goals:" >> displayLn goals
    metavars <- solve goals
    return extract
    where
        solve :: Telescope Extract Judgement -> Refine (Telescope Extract Extract)
        solve = Tl.traverse (local (+1) . refine)

tryTactic :: Judgement -> Refine (ProofState Judgement)
tryTactic j = catchError (run j) (\err -> displayLn err >> tryTactic j)
    where
        run :: Judgement -> Refine (ProofState Judgement)
        run j = do
            str <- getRefinementLine j
            t <- liftEither $ toError $ runParser P.tactic str
            liftError (unTactic t j) 


getRefinementLine :: (MonadRepl m, MonadReader Int m) => Judgement -> m Text
getRefinementLine j = do
    ind <- ask
    l <- getInputLine $ indent ind $ T.append (render j) " "
    case l of
        Just str -> return str
        Nothing -> abort