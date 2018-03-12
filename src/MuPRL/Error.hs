module MuPRL.Error where

import Control.Monad.Trans
import System.Console.ANSI

printErr :: (MonadIO m) => String -> m ()
printErr err = liftIO $ putStr $ prefix ++ err ++ suffix
    where
        prefix = setSGRCode [SetColor Foreground Vivid Red]
        suffix = setSGRCode [Reset]

showErr :: (Show e, MonadIO m) => e -> m ()
showErr err = printErr $ show err ++ "\n"