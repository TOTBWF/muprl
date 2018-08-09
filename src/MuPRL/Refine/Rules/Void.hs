{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Void where

import Control.Monad.Except

import MuPRL.Core.Term
import MuPRL.Refine.Judgement
import MuPRL.Refine.ProofState
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))
import MuPRL.Refine.Rule

-- | Type equality for void
eqType :: (MonadRule m) => Rule m Judgement
eqType = mkRule $ \hyp -> \case
    (Equals Void Void (Universe _)) -> return Axiom
    goal -> ruleMismatch "void/eqtype" (hyp |- goal)