module MuPRL.Error where

import Control.Monad.Except

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import Data.Text (Text)

class Error e where
    errorText :: e -> Doc a

toError :: (Error e) => Either e a -> Either Text a
toError (Left err) = Left $ renderError err
toError (Right r) = Right r

liftExceptT :: (MonadError e' m) => ExceptT e m a -> (e -> e') -> m a
liftExceptT m f = do
    r <- runExceptT m
    case r of
        Left e -> throwError $ f e
        Right a -> return a

liftError :: (Error e, MonadError Text m) => ExceptT e m a -> m a
liftError m = liftExceptT m renderError

-- The issue is that we use monad transformers to hold our errors, but they need to get put into
-- a Textual format. They also need to be able to retain the same base monad...

renderError :: (Error e) => e -> Text
renderError err =
    let errorStyle = color Red <> bold
        errDoc = (annotate errorStyle $ pretty "Error:") <> nest 4 line <> (errorText err)
    in renderStrict $ layoutPretty defaultLayoutOptions errDoc
