{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Equality where

import Control.Monad.Except

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.Core.Term
import MuPRL.Refine.Judgement
import MuPRL.Refine.ProofState
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))
import MuPRL.Refine.Rule

-- | Kind of a funky introduction rule, looks up bindings in the telescope
intro :: (MonadRule m) => Rule m Judgement
intro = mkRule $ \hyp -> \case 
    g@(Equals (Var x) (Var x') t) | x == x' -> 
        case Tl.lookupKey x hyp of
            Just t' | aeq t t' -> return axiomatic
            _ -> ruleMismatch "eq/intro" (hyp |- g)
    g -> ruleMismatch "eq/intro" (hyp |- g)