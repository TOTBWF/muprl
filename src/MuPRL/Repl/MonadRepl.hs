{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MuPRL.Repl.MonadRepl where

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Lazy

import System.Console.Haskeline.MonadException
import qualified System.Console.Haskeline as H
import Unbound.Generics.LocallyNameless.Fresh
import System.Console.ANSI

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text as T
import Data.Text (Text)

import MuPRL.Error

type Repl = ReplT IO

newtype ReplT (m :: * -> *) a = ReplT { unRepl :: H.InputT m a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadException, MonadTrans, MonadRepl)

runReplT :: MonadException m =>  ReplT m a -> m a
runReplT m = H.runInputT H.defaultSettings (H.withInterrupt (unRepl m))

runRepl :: Repl a -> IO a
runRepl = runReplT

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
    getInputLine :: Text -> m (Maybe Text)
    getInputChar :: Text -> m (Maybe Char)
    outputStr    :: Text -> m ()
    outputStrLn  :: Text -> m ()
  
instance MonadException m => MonadRepl (H.InputT m) where
    getInputLine t = (fmap T.pack) <$> (H.getInputLine $ T.unpack t)
    getInputChar t = H.getInputChar $ T.unpack t
    outputStr t = H.outputStr $ T.unpack t
    outputStrLn t = H.outputStrLn $ T.unpack t

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

indent :: Int -> Text -> Text
indent n = T.append (T.replicate (4*n) " ")

render :: (Pretty a) => a -> Text
render a = renderStrict $ layoutPretty defaultLayoutOptions $ pretty a

display :: (MonadRepl m, Pretty a) => a -> m ()
display = outputStr . render

displayLn :: (MonadRepl m, Pretty a) => a -> m ()
displayLn = outputStrLn . render

printError :: (MonadRepl m, Error e) => e -> m ()
printError err = outputStrLn $ renderError err

abort :: (MonadRepl m) => m a
abort = throwIO H.Interrupt

hoistError :: (MonadRepl m, Error e) => Either e a -> m a
hoistError (Left err) = printError err >> abort
hoistError (Right a) = return a
