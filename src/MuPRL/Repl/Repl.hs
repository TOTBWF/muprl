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
import MuPRL.Refine.Tactics
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
        -- Right a -> a

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
    undefined
    -- case res of
    --     Left err -> displayLn err >> tryRule j
    --     Right (ProofState r) -> unbind r
    where
        run :: Judgement -> Refine (ProofState Judgement)
        run j = do
            str <- getRefinementLine j
            t <- liftEither $ toError $ runParser P.tactic str
            -- r <- withExceptT undefined $ (unTactic t) j
            -- (unTactic t) j
            -- t <- liftEither $ toError $ runParser P.tactic str
            -- r <- liftEither $ toError $ (unTactic t) j
            -- r <- (liftEither . toError) =<< ((unTactic t) j)
            undefined

        liftEither :: (MonadError e m) => Either e a ->  m a
        liftEither (Left e) = throwError e
        liftEither (Right a) = return a


getRefinementLine :: (MonadRepl m, MonadReader Int m) => Judgement -> m Text
getRefinementLine j = do
    ind <- ask
    l <- getInputLine $ indent ind $ T.append (render j) " "
    case l of
        Just str -> return str
        Nothing -> abort