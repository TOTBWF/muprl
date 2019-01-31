{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Void where

import Control.Monad.Except

import MuPRL.Core.Term
import MuPRL.Core.Unbound.MonadName
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))
import MuPRL.Refine.Judgement
import MuPRL.Refine.Rule

-- | Type equality for void
eqtype :: (MonadError RuleError m, MonadName m) => TacticT m ()
eqtype = rule $ \hyp -> \case
    (Equals Void Void (Universe _)) -> return Axiom
    goal -> ruleMismatch "void/eqtype" (hyp |- goal)
