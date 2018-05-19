{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Repl.Repl where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

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
import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Telescope (Telescope, (@>))

import MuPRL.Repl.MonadRepl

type Refine = ReaderT Int (FreshMT Repl)

runRefine :: Refine a -> Repl a
runRefine r = runFreshMT $ runReaderT r 0

refine :: Judgement -> Refine Term
refine j = do
    (goals, extract) <- tryRule j
    metavars <- solve goals
    let extract' = Tl.withTelescope metavars extract
    return extract'
    where
        -- Doesn't properly do the subst
        solve :: Telescope Judgement -> Refine (Telescope Term)
        solve = Tl.foldMWithKey (\tl x xj -> (\xt -> tl @> (x,xt)) <$> local (+1) (refine xj)) Tl.empty 

tryRule :: Judgement -> Refine (Telescope Judgement, Term)
tryRule j = do
    res <- run j
    case res of
        Left err -> displayLn err >> tryRule j
        Right (ProofState r) -> unbind r
    where
        run :: Judgement -> Refine (Either Text (ProofState Judgement))
        run j = runExceptT $ do
            str <- getRefinementLine j
            r <- liftEither $ toError $ runParser P.rule str
            (liftEither . toError) =<< runRule j r

        liftEither :: Either e a -> ExceptT e Refine a
        liftEither = either throwError return

getRefinementLine :: (MonadRepl m, MonadReader Int m) => Judgement -> m Text
getRefinementLine j = do
    ind <- ask
    l <- getInputLine $ indent ind $ T.append (render j) " "
    case l of
        Just str -> return str
        Nothing -> abort