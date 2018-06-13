{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Equality where

import Control.Monad.Except

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.Core.Term
import MuPRL.Refine.Judgement
import MuPRL.Refine.ProofState
import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Telescope (Telescope, (@>))
import MuPRL.Refine.Rule

-- | Kind of a funky introduction rule, looks up bindings in the telescope
intro :: (MonadRule m, MonadError RuleError m) => Telescope Term -> Term -> m (ProofState Judgement)
intro hyp (Equals (Var x) (Var x') t) | x == x' && Tl.anyKey (\y t' -> y == x && aeq t t') hyp = return axiomatic
intro hyps goal = ruleMismatch "eq/intro" (hyps |- goal)