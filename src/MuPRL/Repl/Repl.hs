{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Repl.Repl where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import MuPRL.Parser.Stack
import qualified MuPRL.Parser.Tactic as P
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import qualified Data.Text as T
import Data.Text (Text)

import MuPRL.Core.Term

import MuPRL.Error
import MuPRL.Refine.ProofState
import MuPRL.Refine.Judgement
import MuPRL.Refine.Tactic
import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Telescope (Telescope, (@>))

import MuPRL.Repl.MonadRepl

type Refine = ReaderT Int (FreshMT (ExceptT Text Repl))

runRefine :: Refine a -> Repl a
runRefine r = do
    r <- runExceptT $ runFreshMT $ runReaderT r 0
    case r of
        Left err -> outputStrLn err >> abort
        Right a -> return a

refine :: Judgement -> Refine Extract
refine j = do
    (goals, extract) <- tryTactic j
    outputStr "Goals:" >> displayLn goals
    metavars <- solve goals
    -- let extract' = Tl.withTelescope metavars extract
    return extract
    where
        solve :: Telescope Extract Judgement -> Refine (Telescope Extract Extract)
        solve = Tl.traverse (local (+1) . refine)

tryTactic :: Judgement -> Refine (Telescope Extract Judgement, Extract)
tryTactic j = catchError (unbind =<< unProofState <$> run j) (\err -> displayLn err >> tryTactic j)
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