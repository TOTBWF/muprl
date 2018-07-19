{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Universe where

import Control.Monad.Except

import MuPRL.Core.Term
import MuPRL.Refine.Judgement
import MuPRL.Refine.ProofState
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))
import MuPRL.Refine.Rule

-- | Type equality for universes
eqType :: (MonadRule m) => Rule m Judgement
eqType = mkRule $ \hyp -> \case
    (Equals (Universe i) (Universe j) (Universe k)) | (max i j < k) -> return axiomatic
                                                    | otherwise -> throwError $ UniverseMismatch (max i j) k
    goal -> ruleMismatch "universe/eqtype" (hyp |- goal)