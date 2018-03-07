module MuPRL.Parser.Parser where

import Prelude hiding (pi)

import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P 
import qualified Text.Megaparsec.Expr as P
import Unbound.Generics.LocallyNameless
import Data.List (foldl')

import MuPRL.Parser.Lexer
import MuPRL.Syntax

variable :: Parser Var
variable = string2Name <$> identifier

typedef :: Parser (Var, Term)
typedef = (,) <$> variable <*> (colon *> term)

termExpr :: Parser Term
termExpr = P.choice
    [ Var <$> variable
    , reserved "void"  *> pure Void
    , reserved "unit" *> pure Unit
    , reserved "axiom" *> pure Axiom
    , Universe . fromIntegral <$> (reserved "universe" *> integer)
    , lambda <$> (slash *> variable) <*> (dot *> term)
    , uncurry pi <$> (reserved "forall" *> typedef) <*> (dot *> term)
    , parens term
    ]
    where 

appTerm :: Parser Term
appTerm = termExpr >>= \t ->
                        ((\ts -> foldl' App t ts) <$> P.some termExpr)
                        <|> return t

operators :: [[P.Operator Parser Term]]
operators = 
    [ [ P.InfixR (pi <$> (symbol "->" *> fresh wildcardName)) ]
    , [ P.Postfix ((\y a x -> Equals x y a) <$> (equals *> term) <*> (reserved "in" *> term)) ]
    ]

term :: Parser Term
term = P.makeExprParser appTerm operators

runParser :: Parser a -> String -> Either String a
runParser p s = case runFreshM $ P.runParserT p "<stdin>" s of
    (Left err) -> Left $ P.parseErrorPretty err
    (Right r) -> (Right r)