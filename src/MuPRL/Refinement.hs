{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
module MuPRL.Refinement where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad
import Data.Function (fix)

-- Represents a problem with a judgement type j , user state u, Error type e, underlying monad m, and return type a
newtype ProblemT j u e m a = ProblemT { unProblem :: u -> m (Result j u e m a) }

type Problem j u e = ProblemT j u e Identity

-- Represents the result of solving a problem
data Result j u e m a where
    Done :: a -> Result j u e m a
    Fail :: e -> Result j u e m a
    SubProblem :: j b -> Cont j u e m a -> Result j u e m a

data Cont j u e m a 
    = Cont (ProblemT j u e m a)
    | forall b. (b -> a) :<$> (Cont j u e m b)
    | forall b. (Cont j u e m b) :>>= (b -> ProblemT j u e m a)

toProblem :: (Monad m) => Cont j u e m a -> ProblemT j u e m a 
toProblem (Cont problem) = problem
toProblem (m :>>= f) = fuseBind m f
toProblem (f :<$> x) = fuseMap f x

fuseBind :: (Monad m) => Cont j u e m b -> (b -> ProblemT j u e m a) -> ProblemT j u e m a
fuseBind (Cont p) f = p >>= f
fuseBind (g :<$> x) f = fuseBind x (f . g)
fuseBind (m :>>= g) f = fuseBind m (g >=> f)

fuseMap :: (Monad m) => (b -> a) -> Cont j u e m b -> ProblemT j u e m a
fuseMap f (Cont p) = f <$> p
fuseMap f (g :<$> x) = fuseMap (f . g) x
fuseMap f (m :>>= g) = fuseBind m (g >=> return . f)

instance (Monad m) => Functor (ProblemT j u e m) where
    fmap f (ProblemT m) = ProblemT $ \u -> do
        r <- m u
        case r of
            Done a -> return $ Done (f a)
            Fail e -> return $ Fail e
            SubProblem j cont -> return $ SubProblem j (f :<$> cont) --(fmap f . cont)

-- TODO: There is probably a more efficient implementation of this
-- However, things get tricky in the case with 2 Subproblems, as the existential types get... funky
instance (Monad m) => Applicative (ProblemT j u e m) where
    pure = return
    ProblemT ff <*> ProblemT aa = ProblemT $ \u -> do
        rf <- ff u
        case rf of
            Done f -> do
                ra <- aa u
                case ra of
                    Done a -> return $ Done (f a)
                    Fail err -> return $ Fail err
                    SubProblem j acont -> return $ SubProblem j (f :<$> acont)
            Fail err -> return $ Fail err
            SubProblem j fcont -> do
                ra <- aa u
                case ra of
                    Done a -> return $ SubProblem j (($a) :<$> fcont)
                    Fail err -> return $ Fail err
                    SubProblem _ acont -> return $ SubProblem j (Cont (toProblem fcont <*> toProblem acont))

instance (Monad m) => Monad (ProblemT j u e m) where
    return a = ProblemT $ \_ -> return $ Done a
    ProblemT m >>= f = ProblemT $ \u -> do
        r <- m u
        case r of
            Done a -> unProblem (f a) u
            Fail e -> return $ Fail e
            SubProblem j cont -> return $ SubProblem j (cont :>>= f)

instance (Monad m) => MonadReader u (ProblemT j u e m) where
    ask = ProblemT (return . Done)
    local f m = ProblemT $ unProblem m . f

instance (Monad m) => MonadError e (ProblemT j u e m) where
    throwError e = ProblemT (\_ -> return $ Fail e)
    catchError m handler = ProblemT $ \u -> do
        r <- unProblem m u
        case r of
            Fail err -> unProblem (handler err) u
            success -> return success

instance (MonadIO m) => MonadIO (ProblemT j u e m) where
    liftIO = lift . liftIO

instance MonadTrans (ProblemT j u e) where
    lift m = ProblemT $ \_ -> Done <$> m


class (Monad m, MonadError e m) => Decompose j e m | m -> j e where
     decompose :: j a -> m a


runProblemT :: (Monad m, Decompose j e (ProblemT j u e m)) => ProblemT j u e m a -> u -> m (Either e a)
runProblemT p u = do
    r <- unProblem p u
    case r of
        Done a -> return $ Right a
        Fail e -> return $ Left e 
        SubProblem j cont -> do--runProblemT (toProblem . cont =<< decompose j) u
            let x = toProblem cont
            return undefined

runProblem :: (Decompose j e (Problem j u e)) => Problem j u e a -> u -> Either e a
runProblem p u = runIdentity $ runProblemT p u