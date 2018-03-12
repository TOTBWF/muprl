{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MuPRL.Repl where

import Control.Monad.Trans
import System.Console.Haskeline.MonadException
import qualified System.Console.Haskeline as H
import System.Console.ANSI

type Repl = HaskelineT IO

newtype HaskelineT (m :: * -> *) a = HaskelineT { unHaskeline :: H.InputT m a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadException, MonadTrans, MonadHaskeline)

class MonadException m => MonadHaskeline m where
    _getInputLine :: String -> m (Maybe String)
    _getInputChar :: String -> m (Maybe Char)
    _outputStr    :: String -> m ()
    _outputStrLn  :: String -> m ()
  
instance MonadException m => MonadHaskeline (H.InputT m) where
    _getInputLine = H.getInputLine
    _getInputChar = H.getInputChar
    _outputStr = H.outputStr
    _outputStrLn = H.outputStrLn

printErr :: (MonadIO m) => String -> m ()
printErr err = liftIO $ putStr $ prefix ++ err ++ suffix
    where
        prefix = setSGRCode [SetColor Foreground Vivid Red]
        suffix = setSGRCode [Reset]

getInputLine :: (MonadException m) => String -> HaskelineT m String
getInputLine banner = do
    l <- H.handleInterrupt (return (Just "")) $ _getInputLine banner
    case l of
        Just line -> return line
        Nothing -> abort

indent :: Int -> String -> String
indent n str = replicate (4*n) ' ' ++ str

showErr :: (Show e, MonadIO m) => e -> m ()
showErr err = printErr $ show err ++ "\n"

abort :: (MonadIO m) => HaskelineT m a
abort = throwIO H.Interrupt
