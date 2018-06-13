{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Void where

import Control.Monad.Except

import MuPRL.Core.Term
import MuPRL.Refine.Judgement
import MuPRL.Refine.ProofState
import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Telescope (Telescope, (@>))
import MuPRL.Refine.Rule

-- | Type equality for void
eqType :: (MonadRule m, MonadError RuleError m) => Telescope Term -> Term -> m (ProofState Judgement)
eqType _ (Equals Void Void (Universe _)) = return axiomatic
eqType hyps goal = ruleMismatch "void/eqtype" (hyps |- goal)