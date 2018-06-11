module MuPRL.Error where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import Control.Monad.Except
import Data.Text (Text)

class Error e where
    errorText :: e -> Doc a

-- toError (Left err) = Left $ renderError err
-- toError (Right r) = Right r

renderError :: (Error e) => e -> Text
renderError err =
    let errorStyle = color Red <> bold
        errDoc = (annotate errorStyle $ pretty "Error:") <> nest 4 line <> (errorText err)
    in renderStrict $ layoutPretty defaultLayoutOptions errDoc
