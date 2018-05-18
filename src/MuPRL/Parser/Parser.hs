{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Parser.Parser where

import Prelude hiding (pi)

import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Text.Megaparsec as P 
import qualified Text.Megaparsec.Expr as P
import Unbound.Generics.LocallyNameless
import Data.List (foldl')

import Data.Text (Text)
import qualified Data.Text as T

import MuPRL.Parser.Lexer
import MuPRL.Parser.Stack

import MuPRL.Core.Term

import MuPRL.Refine.Rules
import MuPRL.Refine.ProofState

variable :: Parser Var
variable = (string2Name . T.unpack) <$> identifier

typedef :: Parser (Var, Term)
typedef = (,) <$> variable <*> (colon *> term)

termExpr :: Parser Term
termExpr = P.choice
    [ Var <$> variable
    , reserved "void"  $> Void
    -- , reserved "unit" $> Unit
    -- , reserved "nil" $> Nil
    , reserved "axiom" $> Axiom
    , Universe . fromIntegral <$> (reserved "universe" *> integer)
    , lambda <$> (slash *> variable) <*> (dot *> term)
    , P.try $ uncurry pi <$> parens typedef <*> (symbol "->" *> term)
    , parens term
    ]

appTerm :: Parser Term
appTerm = termExpr >>= \t ->
                        (foldl' App t <$> P.some termExpr)
                        <|> return t

operators :: [[P.Operator Parser Term]]
operators = 
    [ [ P.InfixR (pi <$> (symbol "->" *> wildcardName)) ]
    -- , [ P.InfixR (symbol "*" *> pure Prod) ]
    -- , [ P.InfixR (symbol "+" *> pure Sum) ]
    -- , [ P.Postfix ((\y a x -> Equals x y a) <$> (equals *> term) <*> (reserved "in" *> term)) 
    --   , P.Postfix (flip eqRefl <$> (reserved "in" *> term))]
    ]

term :: Parser Term
term = P.makeExprParser appTerm operators

rule :: (MonadRule m) => Parser (Rule m Judgement)
rule = reserved "by" *> P.choice 
    [ reserved "assumption" $> assumption
    -- , introApp <$> (reserved "intro-app" *> reserved "using" *> term)
    , reserved "intro" $> intro
    ]
    -- [ reserved "hypothesis" *> pure hypothesis
    -- , reserved "intro-void" *> pure introVoid
    -- , reserved "intro-unit" *> pure introUnit
    -- , reserved "intro-nil" *> pure introNil
    -- , reserved "intro-universe" *> pure introUniverse
    -- ]



