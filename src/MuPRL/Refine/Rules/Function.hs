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
intro :: (MonadRule m) => Rule m Judgement
intro = mkRule $ \hyp -> \case
    (Pi bnd) -> do
        ((x, unembed -> a), bx) <- unbind bnd
        -- We first need to check the well formedness of 'a'
        (wGoal, _) <- wellFormed hyp a
        (bGoal, body) <- goal (Tl.extend x a hyp |- bx)
        return ((Tl.empty @> wGoal @> bGoal) |> lambda x body)
    goal -> ruleMismatch "fun/intro" (hyp |- goal)

-- | Non-extensional function equality
eq :: (MonadRule m) => Rule m Judgement
eq = mkRule $ \hyp -> \case
    (Equals (Lam lbnd1) (Lam lbnd2) (Pi pbnd)) -> do
        (x, body1) <- unbind lbnd1
        (y, body2) <- unbind lbnd2
        ((z, unembed -> a), b) <- unbind pbnd
        -- Make sure that the variables actually reference the right thing
        let body1' = subst x (Var z) body1
        let body2' = subst y (Var z) body2
        (wGoal, _) <- wellFormed hyp a
        (bodyGoal, _) <- goal (Tl.extend z a hyp |- (Equals body1' body2' b))
        return (Tl.empty @> wGoal @> bodyGoal |> Axiom)
    goal -> ruleMismatch "fun/eq" (hyp |- goal)

-- | Type equality for functions
eqType :: (MonadRule m) => Rule m Judgement
eqType = mkRule $ \hyp -> \case
    (Equals (Pi bnd1) (Pi bnd2) u@(Universe k)) | aeq bnd1 bnd2 -> do
        ((x, unembed -> a), b) <- unbind bnd1
        (aGoal, _) <- goal (hyp |- (Equals a a u))
        (bGoal, _) <- goal (Tl.extend x a hyp |- (Equals b b u))
        return ((Tl.empty @> bGoal @> aGoal) |> Axiom)
    goal -> ruleMismatch "fun/eqtype" (hyp |- goal)

elim :: (MonadRule m) => Name Term -> Rule m Judgement
elim f = mkRule $ \hyp g ->
    case Tl.findKey f hyp of
        Just (Pi bnd) -> do
            ((_, unembed -> a), b) <- unbind bnd
            (aGoal, aHole) <- goal (hyp |- a)
            f' <- fresh f
            (bGoal, bHole) <- goal (hyp @> (f',b) |- g)
            let extract = subst f' (App (Var f) aHole) bHole
            return (Tl.empty @> bGoal @> aGoal |> extract)
        Just t -> throwError $ ElimMismatch "fun/elim" t
        Nothing -> throwError $ UndefinedVariable f
