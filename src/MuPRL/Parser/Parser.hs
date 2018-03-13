module MuPRL.Parser.Parser where

import Prelude hiding (pi)

import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P 
import qualified Text.Megaparsec.Expr as P
import Unbound.Generics.LocallyNameless
import Data.List (foldl')

import MuPRL.Parser.Lexer
import MuPRL.Syntax
import MuPRL.Rules

variable :: Parser Var
variable = string2Name <$> identifier

typedef :: Parser (Var, Term)
typedef = (,) <$> variable <*> (colon *> term)

termExpr :: Parser Term
termExpr = P.choice
    [ Var <$> variable
    , reserved "void"  *> pure Void
    , reserved "unit" *> pure Unit
    , reserved "nil" *> pure Nil
    , reserved "axiom" *> pure Axiom
    , Universe . fromIntegral <$> (reserved "universe" *> integer)
    , lambda <$> (slash *> variable) <*> (dot *> term)
    , P.try $ uncurry pi <$> (parens typedef) <*> (symbol "->" *> term)
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
    , [ P.InfixR (symbol "*" *> pure Prod) ]
    , [ P.InfixR (symbol "+" *> pure Sum) ]
    , [ P.Postfix ((\y a x -> Equals x y a) <$> (equals *> term) <*> (reserved "in" *> term)) 
      , P.Postfix ((flip eqRefl) <$> (reserved "in" *> term))]
    ]

term :: Parser Term
term = P.makeExprParser appTerm operators

rule :: (MonadRule m env) => Parser (Rule m)
rule = reserved "by" *> P.choice 
    [ reserved "hypothesis" *> pure hypothesis
    , reserved "intro-void" *> pure introVoid
    , reserved "intro-unit" *> pure introUnit
    , reserved "intro-nil" *> pure introNil
    , reserved "intro-universe" *> pure introUniverse
    , introApp <$> (reserved "intro-app" *> reserved "using" *> term)
    , introLambda . fromInteger <$> (reserved "intro-lambda" *> reserved "at" *> integer) <*> (reserved "new" *> variable)
    ]



runParser :: Parser a -> String -> Either String a
runParser p s = case runFreshM $ P.runParserT p "<stdin>" s of
    (Left err) -> Left $ P.parseErrorPretty err
    (Right r) -> (Right r)