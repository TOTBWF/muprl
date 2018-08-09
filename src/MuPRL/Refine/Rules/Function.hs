{-# LANGUAGE OverloadedStrings #-}
module MuPRL.Refine.Rules.Function where

import Control.Monad.Except

import MuPRL.Core.Term
import MuPRL.Core.Unbound
import MuPRL.Core.Unbound.MonadName
import MuPRL.Refine.Judgement
import MuPRL.Refine.ProofState
import qualified MuPRL.Core.Telescope as Tl
import MuPRL.Core.Telescope (Telescope, (@>))
import MuPRL.Refine.Rule

import Debug.Trace

-- | Introduction rule for functions
intro :: (MonadRule m) => Rule m Judgement
intro = mkRule $ \hyp -> \case
    (Pi bnd) -> lunbind bnd $ \(x, a, b) -> do
        bodyHole <- goal (hyp @> (x, a) |- b)
        wellFormed hyp a
        return $ lambda x bodyHole
    goal -> ruleMismatch "fun/intro" (hyp |- goal)

-- | Non-extensional function equality
eq :: (MonadRule m) => Rule m Judgement
eq = mkRule $ \hyp -> \case
    (Equals (Lam lbnd1) (Lam lbnd2) (Pi pbnd)) -> do
        -- (x, body1) <- unbind lbnd1
        -- (y, body2) <- unbind lbnd2
        -- ((z, unembed -> a), b) <- unbind pbnd
        -- ((z, unembed -> a), b) <- unbind pbnd
        -- z' <- metavar z
        -- Make sure that the variables actually reference the right thing
        -- let body1' = subst x (hole z) body1
        -- let body2' = subst y (hole z) body2
        -- (wGoal, _) <- wellFormed hyp a
        -- (bodyGoal, _) <- goal (hyp @> (z, a) |- (Equals body1' body2' b))
        -- return (Tl.empty @> wGoal @> bodyGoal |>> Axiom)
        undefined
    goal -> ruleMismatch "fun/eq" (hyp |- goal)

-- | Type equality for functions
eqType :: (MonadRule m) => Rule m Judgement
eqType = mkRule $ \hyp -> \case
    (Equals (Pi bnd1) (Pi bnd2) u@(Universe k)) | aeq bnd1 bnd2 -> lunbind bnd1 $ \(x, a, b) -> do
        _ <- goal (hyp |- (Equals a a u))
        _ <- goal (hyp @> (x,a) |- (Equals b b u))
        return Axiom
    goal -> ruleMismatch "fun/eqtype" (hyp |- goal)

elim :: (MonadRule m) => Var -> Rule m Judgement
elim f = mkRule $ \hyp g -> 
    case Tl.lookupKey f hyp of
        Just p@(Pi _) -> do
            -- The hard part about this is that 'x:a -> b -> c -> d' should have
            -- '(((x _a) _b) _c)' as it's extract, and the way we handle substitutions WRT metavariables is bad
            (_, extract, outputTy) <- elimArgs (hyp, Var f) p
            -- This is where things get tricky. We want to be able to show that
            -- given a term of type 'd', we can prove our original goal
            x <- var wildcard
            -- (mGoal, mHole) <- goal (hyp @> (x,outputTy) |- g)
            -- return (goals @> mGoal |>> subst x extract mHole)
            (Hole _ mHole) <- goal (hyp @> (x,outputTy) |- g)
            return $ Hole (MetaSubst [(x, extract)] (DelayedBinds [])) mHole
        Just t -> throwError $ ElimMismatch "fun/elim" t
        Nothing -> throwError $ UndefinedVariable f

    where
        elimArgs (hyp, extract) (Pi bnd) = lunbind bnd $ \(x, a, b) -> do
            -- This just recurses down the function type, adding goals and building up our extract term
            aHole <- goal (hyp |- a)
            let extract' = (App extract aHole)
            elimArgs (hyp @> (x,a), extract') b
        elimArgs (hyp, extract) t = return (hyp, extract, t)
