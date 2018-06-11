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
import MuPRL.Refine.Rules (MonadRule, Rule, mkRule)
import MuPRL.Refine.Tactics (MonadTactic, Tactic)
import qualified MuPRL.Refine.Rules as R
import qualified MuPRL.Refine.Tactics as R


rule :: (MonadRule m) => Parser (Rule m Judgement)
rule = reserved "by" *> P.choice 
    [ reserved "assumption" $> (R.mkRule R.assumption)
    , reserved "intro" $> (mkRule R.intro)
    ]

tactic' :: (MonadRule m, MonadTactic m) => Parser (Tactic m Judgement)
tactic' = P.choice
    [ reserved "id" $> R.idt
    , reserved "try" *> tactic
    , reserved "fail" $> R.fail "TODO"
    , R.rule <$> (reserved "rule" *> rule)
    ]

operators :: (MonadRule m, MonadTactic m) => [[P.Operator Parser (Tactic m Judgement)]]
operators =
    [ [ P.InfixR (R.then_ <$ symbol ";") ]
    ]

tactic :: (MonadRule m, MonadTactic m) => Parser (Tactic m Judgement)
tactic = P.makeExprParser tactic' operators