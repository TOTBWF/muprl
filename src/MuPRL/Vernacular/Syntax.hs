module MuPRL.Vernacular.Syntax where

import Data.Text (Text)

import MuPRL.Core.Term

import MuPRL.Refine.Judgement
import MuPRL.Refine.Tactic

data Vernacular m
    = Theorem Text Term (Tactic m Judgement)

