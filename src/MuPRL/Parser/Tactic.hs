{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Parser.Tactic where

import Prelude hiding (pi)

import Data.Function ((&))
import Control.Applicative ((<|>))
import Control.Monad.Except
import Data.Functor (($>))
import qualified Text.Megaparsec as P 
import qualified Text.Megaparsec.Expr as P
import Unbound.Generics.LocallyNameless
import Data.List (foldl')

import Data.Text (Text)
import qualified Data.Text as T

import MuPRL.Parser.Lexer
import MuPRL.Parser.Stack
import MuPRL.Parser.Term (term, variable)

import MuPRL.Core.Term

import Refinery.Tactic ((<@>), (<!>))
import qualified Refinery.Tactic as T

import MuPRL.Core.Unbound.MonadName
import MuPRL.Refine.Judgement
import MuPRL.Refine.Rule (TacticT, RuleError)
import qualified MuPRL.Refine.Rule as R
import qualified MuPRL.Refine.Tactic as T

rule :: (MonadError RuleError m, MonadName m) => Parser (TacticT m ())
rule = P.choice
    [ reserved "id" $> (pure ())
    , T.try <$> (reserved "try" *> tactic)
    -- , reserved "fail" $> R.fail "fail tactic invoked"
    , reserved "intro" $> T.intro
    , reserved "eqtype" $> T.eqtype
    , reserved "assumption" $> T.assumption
    , T.elim <$> (reserved "elim" *> identifier)
    , T.named <$> (reserved "rule" *> ruleName)
    , braces tactic
    ]

multiTactic :: (MonadError RuleError m, MonadName m) => Parser (TacticT m () -> TacticT m ())
multiTactic = P.choice
    [ flip (<@>) <$> brackets (P.sepBy tactic comma)
    , flip (>>) <$> tactic
    ]

operators :: (MonadError RuleError m, MonadName m) => [[P.Operator Parser (TacticT m ())]]
operators =
    [ [ P.Prefix (symbol "*" $> T.many_) ]
    , [ P.InfixR (symbol "|" $> (<!>)) ]
    ]

singleTactic :: (MonadError RuleError m, MonadName m) => Parser (TacticT m ())
singleTactic = P.makeExprParser rule operators

tactic :: (MonadError RuleError m, MonadName m) => Parser (TacticT m ())
tactic = do
  t <- singleTactic
  ts <- P.many (symbol ";" *> multiTactic)
  return $ foldl (&) t ts
