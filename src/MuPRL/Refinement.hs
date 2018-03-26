module MuPRL.Refinement where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import Unbound.Generics.LocallyNameless

import MuPRL.LCF
import MuPRL.Rules
import MuPRL.Syntax
import MuPRL.Environment
import MuPRL.PrettyPrint
import MuPRL.Repl
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..))
import Data.Bifunctor
import qualified MuPRL.Parser.Parser as P

type Refinement = ReaderT Env (FreshMT (ExceptT RuleError Repl))

runRefinement :: Refinement a -> Repl a
runRefinement r = do
    res <- runExceptT $ runFreshMT $ runReaderT r emptyEnv
    case res of
        Right x -> return x
        Left err -> showErr err >> abort

refine :: Judgement -> Refinement Term
refine t = do
    (goals :#> e) <- getRule t
    -- For each one of the subgoals, we have to solve it, then fill in the hole for the rest of the telescope
    metavars <- solve goals
    -- Use our computed metavariables to fill in all of the free vars in the extract term
    return $ foldl (\e (v,t) -> subst v t e) e metavars 
    where
        solve :: Seq (Var, Judgement) -> Refinement (Seq (Var, Term))
        solve Empty = return Empty
        solve ((x, jdg) :<| xs) = do
            t <- refine jdg
            ts <- solve $ fmap (second (substJudgement x t)) xs
            return $ (x,t) :<| ts


getRule :: Judgement -> Refinement ProofState
getRule t = do
    str <- getRefinementLine t
    case P.runParser P.rule str of
        (Right r) -> catchError ((unRule r) t) (\err -> showErr err >> getRule t)
        (Left err) -> do
            printErr err
            getRule t

getRefinementLine :: Judgement -> Refinement String
getRefinementLine t = do
    env <- ask
    l <- getInputLine $ indent (envLevel env) $ pp t ++ " "
    case l of
        Just str -> return str
        Nothing -> abort