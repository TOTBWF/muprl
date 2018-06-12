{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Parser.Vernacular where

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
import MuPRL.Parser.Term
import MuPRL.Parser.Tactic

import MuPRL.Core.Term

import MuPRL.Refine.Rule (MonadRule, Rule, mkRule)
import MuPRL.Refine.Tactic (Tactic)
import qualified MuPRL.Refine.Rule as R
import qualified MuPRL.Refine.Tactic as R

import MuPRL.Vernacular.Syntax

theorem :: (MonadRule m) => Parser (Vernacular m)
theorem = reserved "Theorem" *> (Theorem <$> identifier <*> (colon *> term) <*> braces tactic)

vernacular :: (MonadRule m) => Parser [Vernacular m]
vernacular = P.many theorem