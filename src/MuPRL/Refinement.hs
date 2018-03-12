module MuPRL.Refinement where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import Unbound.Generics.LocallyNameless
import System.Exit

import MuPRL.Rules
import MuPRL.Syntax
import MuPRL.Environment
import MuPRL.PrettyPrint
import MuPRL.Parser.Parser

type Refinement = ReaderT Env (FreshMT (ExceptT RuleError IO))

runRefinement :: Refinement a -> IO a
runRefinement r = do
    res <- runExceptT $ runFreshMT $ runReaderT r emptyEnv
    case res of
        Right x -> return x
        Left err -> die $ show err

refine :: Term -> Refinement ()
refine t = do
    goals <- getRule t
    -- goals <- catchError (applyRule t r) (handleError)
    mapM_ (\(ctx' :>> t') -> local (\env -> env { envLevel=(1 + envLevel env), envProofState=ctx' } ) (refine t')) goals
    -- where
    --     handleError :: RuleError -> Refinement ()
    --     handleError err = do
    --         liftIO $ print err
    --         refine t


getRule :: Term -> Refinement [Goal]
getRule t = do
    env <- ask
    let g = (envProofState env :>> t)
    let l = (envLevel env)
    liftIO $ putStrLn $ replicate (l*4) ' ' ++ pp g
    str <- liftIO $ getLine
    case runParser rule str of
        (Right r) -> do
            catchError (applyRule t r) (\err -> (liftIO $ print err) >> getRule t)
        (Left err) -> do
            liftIO $ putStrLn $ show err
            getRule t
    -- where hoistErr :: (Show e)