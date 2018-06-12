{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Parser.Tactic where

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

import MuPRL.Refine.ProofState
import MuPRL.Refine.Judgement
import MuPRL.Refine.Rule (MonadRule, Rule, mkRule)
import MuPRL.Refine.Tactic (Tactic)
import qualified MuPRL.Refine.Rule as R
import qualified MuPRL.Refine.Tactic as R


rule :: (MonadRule m) => Parser (Rule m Judgement)
rule = P.choice 
    [ reserved "assumption" $> (R.mkRule R.assumption)
    , reserved "intro" $> (mkRule R.intro)
    ]

tactic' :: (MonadRule m) => Parser (Tactic m Judgement)
tactic' = P.choice
    [ reserved "id" $> R.idt
    , reserved "try" *> tactic
    , reserved "fail" $> R.fail "fail tactic invoked"
    , R.rule <$> (reserved "rule" *> rule)
    ]

multitactic :: (MonadRule m) => Parser (Tactic m (ProofState Judgement))
multitactic = P.choice
    [ R.each <$> brackets (P.sepBy tactic comma)
    , R.all_ <$> tactic
    ]

operators :: (MonadRule m) => [[P.Operator Parser (Tactic m Judgement)]]
operators =
    [ [ P.Postfix (symbol ";" *> (flip R.seq_ <$> multitactic)) ]
    ]

tactic :: (MonadRule m) => Parser (Tactic m Judgement)
tactic = P.makeExprParser tactic' operators