module MuPRL.Refinement where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import Unbound.Generics.LocallyNameless

import MuPRL.Rules
import MuPRL.Syntax
import MuPRL.Environment
import MuPRL.PrettyPrint
import MuPRL.Repl
import MuPRL.Parser.Parser

type Refinement = ReaderT Env (FreshMT (ExceptT RuleError Repl))

runRefinement :: Refinement a -> Repl a
runRefinement r = do
    res <- runExceptT $ runFreshMT $ runReaderT r emptyEnv
    case res of
        Right x -> return x
        Left err -> showErr err >> abort

refine :: Term -> Refinement ()
refine t = do
    goals <- getRule t
    mapM_ (\(ctx' :>> t') -> local (\env -> env { envLevel=(1 + envLevel env), envProofState=ctx' } ) (refine t')) goals


getRule :: Term -> Refinement [Goal]
getRule t = do
    str <- getRefinementLine t
    case runParser rule str of
        (Right r) -> do
            catchError (applyRule t r) (\err -> (showErr err) >> getRule t)
        (Left err) -> do
            printErr err
            getRule t

getRefinementLine :: Term -> Refinement String
getRefinementLine t = do
    env <- ask
    l <- getInputLine $ indent (envLevel env) $ pp (envProofState env :>> t) ++ " "
    case l of
        Just str -> return str
        Nothing -> abort