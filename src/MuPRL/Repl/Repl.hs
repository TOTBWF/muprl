{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Repl.Repl where

import Control.Monad.Except

import MuPRL.Parser.Stack
import qualified MuPRL.Parser.Parser as P
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import qualified Data.Text as T
import Data.Text (Text)

import MuPRL.Core.Term

import MuPRL.Error
import MuPRL.Refine.ProofState
import MuPRL.Refine.Rules
import MuPRL.Refine.Telescope

import MuPRL.Repl.MonadRepl

type Refine = FreshMT Repl

runRefine :: Refine a -> Repl a
runRefine = runFreshMT 

refine :: Judgement -> Refine Term
refine j = do
    (goals :#> e) <- tryRule j
    metavars <- solve goals
    return $ foldlVars (\e v t -> subst v t e) e metavars
    where
        solve :: Telescope Judgement -> Refine (Telescope Term)
            -- TODO: Substitute judgments
        solve = traverse refine
            -- t <- refine jdg
            -- ts <- solve $ fmap (substJdg 

tryRule :: Judgement -> Refine (ProofState Judgement)
tryRule j = do
    res <- run j
    case res of
        Left err -> displayLn err >> tryRule j
        Right r -> return r
    where
        run :: Judgement -> Refine (Either Text (ProofState Judgement))
        run j = runExceptT $ do
            str <- getRefinementLine j
            r <-  liftEither $ toError $ runParser P.rule str
            (liftEither . toError) =<< runRule j r

        liftEither :: Either e a -> ExceptT e Refine a
        liftEither = either throwError return

getRefinementLine :: (MonadRepl m) => Judgement -> m Text
getRefinementLine j = do
    l <- getInputLine $ T.append (render j) " "
    case l of
        Just str -> return str
        Nothing -> abort