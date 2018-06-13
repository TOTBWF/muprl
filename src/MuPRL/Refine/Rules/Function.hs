{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Function where

import Control.Monad.Except

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh

import MuPRL.Core.Term
import MuPRL.Refine.Judgement
import MuPRL.Refine.ProofState
import qualified MuPRL.Refine.Telescope as Tl
import MuPRL.Refine.Telescope (Telescope, (@>))
import MuPRL.Refine.Rule

-- | Introduction rule for functions
intro :: (MonadRule m, MonadError RuleError m) => Telescope Term -> Term -> m (ProofState Judgement)
intro hyp (Pi bnd) = do
    ((x, unembed -> a), bx) <- unbind bnd
    -- We first need to check the well formedness of 'a'
    (wGoal, _) <- wellFormed hyp a
    (bGoal, body) <- goal (Tl.extend x a hyp |- bx)
    return ((Tl.empty @> wGoal @> bGoal) |> lambda x body)
intro hyps goal = ruleMismatch "fun/intro" (hyps |- goal)

-- | Type equality for functions
eqType :: (MonadRule m, MonadError RuleError m) => Telescope Term -> Term -> m (ProofState Judgement)
eqType hyp (Equals (Pi bnd1) (Pi bnd2) u@(Universe k)) | aeq bnd1 bnd2 = do
    ((x, unembed -> a), b) <- unbind bnd1
    (aGoal, _) <- goal (hyp |- (Equals a a u))
    (bGoal, _) <- goal (Tl.extend x a hyp |- (Equals b b u))
    return ((Tl.empty @> bGoal @> aGoal) |> Axiom)
eqType hyps goal = ruleMismatch "fun/eqtype" (hyps |- goal)