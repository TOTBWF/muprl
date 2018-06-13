{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Universe where

import Control.Monad.Except

import MuPRL.Core.Term
import MuPRL.Refine.Judgement
import MuPRL.Refine.ProofState
import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Telescope (Telescope, (@>))
import MuPRL.Refine.Rule

-- | Type equality for universes
eqType :: (MonadRule m, MonadError RuleError m) => Telescope Term -> Term -> m (ProofState Judgement)
eqType _ (Equals (Universe i) (Universe j) (Universe k)) | (max i j < k) = return axiomatic
                                                         | otherwise = throwError $ UniverseMismatch (max i j) k
eqType hyps goal = ruleMismatch "universe/eqtype" (hyps |- goal)