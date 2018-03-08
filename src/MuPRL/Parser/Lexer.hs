module MuPRL.Parser.Lexer where
    
import Data.Functor (void)
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad.Trans
import Unbound.Generics.LocallyNameless

type Parser = P.ParsecT Void String FreshM

instance Fresh Parser where
    fresh = lift . fresh

lineCmt :: Parser ()
lineCmt = L.skipLineComment "--"

blockCmt :: Parser ()
blockCmt = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space P.space1 lineCmt blockCmt

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

symbol' :: String -> Parser ()
symbol' = void . symbol 

integer :: Parser Integer
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

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

reserved :: String -> Parser ()
reserved s = lexeme (P.string s *> P.notFollowedBy P.alphaNumChar)

identifier :: Parser String
identifier = (lexeme . P.try) (ident >>= checkReserved)
    where
        ident = (:) <$> P.lowerChar <*> (P.many (P.alphaNumChar <|> P.char '\''))

uidentifier :: Parser String
uidentifier = (lexeme . P.try) (ident >>= checkReserved)
    where
        ident = (:) <$> P.upperChar <*> (P.many (P.alphaNumChar <|> P.char '\''))

checkReserved :: String -> Parser String
checkReserved i = if i `elem` reservedWords
                    then fail $ "reserved word " ++ i ++ " is not a valid identifier"
                    else return i

reservedWords :: [String]
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