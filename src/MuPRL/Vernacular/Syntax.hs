module MuPRL.Vernacular.Syntax where

import Control.Monad.Except

import Data.Text (Text)

import MuPRL.Core.Term

import MuPRL.Core.Unbound.MonadName
import MuPRL.Refine.Judgement
import MuPRL.Refine.Rule
import MuPRL.Refine.Tactic

data Vernacular m
    = Theorem Text Term (TacticT (NameMT (ExceptT RuleError m)) ())
