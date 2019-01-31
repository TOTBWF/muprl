{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Universe where

import Control.Monad.Except

import MuPRL.Core.Term
import MuPRL.Core.Unbound.MonadName
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))
import MuPRL.Refine.Judgement
import MuPRL.Refine.Rule

-- | Type equality for universes
eqtype :: (MonadError RuleError m, MonadName m) => TacticT m ()
eqtype = rule $ \hyp -> \case
    (Equals (Universe i) (Universe j) (Universe k)) | (max i j < k) -> return Axiom
                                                    | otherwise -> throwError $ UniverseMismatch (max i j) k
    goal -> ruleMismatch "universe/eqtype" (hyp |- goal)
