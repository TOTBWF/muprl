{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Parser.Lexer where
    
import Data.Functor (void)
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text)
import qualified Data.Text as T

import MuPRL.Parser.Stack

lineCmt :: Parser ()
lineCmt = L.skipLineComment "--"

blockCmt :: Parser ()
blockCmt = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space P.space1 lineCmt blockCmt

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Text -> Parser ()
symbol' = void . symbol 

integer :: Parser Integer
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = P.between (symbol "[") (symbol "]")

arrow :: Parser ()
arrow = symbol' "->"

slash :: Parser ()
slash = symbol' "\\"

equals :: Parser ()
equals = symbol' "="

dot :: Parser ()
dot = symbol' "."

colon :: Parser ()
colon = symbol' ":"

reserved :: Text -> Parser ()
reserved s = lexeme (P.string s *> P.notFollowedBy P.alphaNumChar)

identifier :: Parser Text
identifier = (lexeme . P.try) (ident >>= checkReserved)
    where
        ident = (T.cons) <$> P.lowerChar <*> (T.pack <$> P.many (P.alphaNumChar <|> P.char '\''))

uidentifier :: Parser Text
uidentifier = (lexeme . P.try) (ident >>= checkReserved)
    where
        ident = (T.cons) <$> P.upperChar <*> (T.pack <$> P.many (P.alphaNumChar <|> P.char '\''))

checkReserved :: Text -> Parser Text
checkReserved i = if i `elem` reservedWords
                    then fail $ "reserved word " ++ (T.unpack i) ++ " is not a valid identifier"
                    else return i

reservedWords :: [Text]
reservedWords = 
    [ "in"
    , "void"
    , "unit"
    , "nil"
    , "universe"
    , "forall"
    , "axiom"
    , "by"
    , "using"
    ]