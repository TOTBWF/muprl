module MuPRL.Parser.Stack where

import Control.Monad.Trans
import Data.Void (Void)

import qualified Text.Megaparsec as P
import Unbound.Generics.LocallyNameless.Fresh

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

import MuPRL.Error

type Parser = P.ParsecT Void Text FreshM

newtype ParseError = ParseError (P.ParseError (P.Token Text) Void)

instance Error ParseError where
    errorText (ParseError err) = pretty $ P.parseErrorPretty err

runParser :: Parser a -> Text -> Either ParseError a
runParser p s = case runFreshM $ P.runParserT p "<stdin>" s of
    (Left err) -> Left $ ParseError err
    (Right r) -> Right r

instance Fresh Parser where
    fresh = lift . fresh