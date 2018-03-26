{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MuPRL.Repl where

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Lazy
import Unbound.Generics.LocallyNameless
import System.Console.Haskeline.MonadException
import qualified System.Console.Haskeline as H
import System.Console.ANSI
import System.Exit

type Repl = ReplT IO

newtype ReplT (m :: * -> *) a = ReplT { unRepl :: H.InputT m a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadException, MonadTrans, MonadRepl)

runReplT :: MonadException m =>  ReplT m a -> m a
runReplT m = H.runInputT H.defaultSettings (H.withInterrupt (unRepl m))

-- | Boilerplate MTL definitions
instance MonadException m => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
        run' = RunIO (fmap ExceptT . run . runExceptT)
        in runExceptT <$> f run'

instance MonadException m => MonadException (FreshMT m) where
    controlIO f = FreshMT $ controlIO $ \(RunIO run) -> let
        run' = RunIO (fmap FreshMT . run . unFreshMT)
        in unFreshMT <$> f run'


instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in (`runStateT` s) <$> f run'

class MonadException m => MonadRepl m where
    getInputLine :: String -> m (Maybe String)
    getInputChar :: String -> m (Maybe Char)
    outputStr    :: String -> m ()
    outputStrLn  :: String -> m ()
  
instance MonadException m => MonadRepl (H.InputT m) where
    getInputLine = H.getInputLine
    getInputChar = H.getInputChar
    outputStr = H.outputStr
    outputStrLn = H.outputStrLn

instance MonadRepl m => MonadRepl (ReaderT r m) where
    getInputLine = lift . getInputLine
    getInputChar = lift . getInputChar
    outputStr = lift . outputStr
    outputStrLn = lift . outputStrLn

instance MonadRepl m => MonadRepl (ExceptT e m) where
    getInputLine = lift . getInputLine
    getInputChar = lift . getInputChar
    outputStr = lift . outputStr
    outputStrLn = lift . outputStrLn

instance MonadRepl m => MonadRepl (FreshMT m) where
    getInputLine = lift . getInputLine
    getInputChar = lift . getInputChar
    outputStr = lift . outputStr
    outputStrLn = lift . outputStrLn

printErr :: (MonadIO m) => String -> m ()
printErr err = liftIO $ putStr $ prefix ++ err ++ suffix
    where
        prefix = setSGRCode [SetColor Foreground Vivid Red]
        suffix = setSGRCode [Reset]

indent :: Int -> String -> String
indent n str = replicate (4*n) ' ' ++ str

showErr :: (Show e, MonadIO m) => e -> m ()
showErr err = printErr $ show err ++ "\n"

abort :: (MonadRepl m) => m a
abort = throwIO H.Interrupt
