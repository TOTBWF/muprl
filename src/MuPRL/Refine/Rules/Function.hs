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

import Debug.Trace

-- | Introduction rule for functions
intro :: (MonadRule m) => Rule m Judgement
intro = mkRule $ \hyp -> \case
    (Pi bnd) -> do
        ((x, unembed -> a), b) <- unbind bnd
        -- (x, a, b) <- unbindVar bnd
        -- x' <- var x
        -- We first need to check the well formedness of 'a'
        (wGoal, _) <- wellFormed hyp a
        (bGoal, body) <- goal (hyp @> (x, a) |- b)
        return ((Tl.empty @> wGoal @> bGoal) |>> lambda x body)
    goal -> ruleMismatch "fun/intro" (hyp |- goal)

-- | Non-extensional function equality
eq :: (MonadRule m) => Rule m Judgement
eq = mkRule $ \hyp -> \case
    (Equals (Lam lbnd1) (Lam lbnd2) (Pi pbnd)) -> do
        (x, body1) <- unbind lbnd1
        (y, body2) <- unbind lbnd2
        ((z, unembed -> a), b) <- unbind pbnd
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
    (Equals (Pi bnd1) (Pi bnd2) u@(Universe k)) | aeq bnd1 bnd2 -> do
        ((x, unembed -> a), b) <- unbind bnd1
        (aGoal, _) <- goal (hyp |- (Equals a a u))
        (bGoal, _) <- goal (hyp @> (x,a) |- (Equals b b u))
        return ((Tl.empty @> bGoal @> aGoal) |>> Axiom)
    goal -> ruleMismatch "fun/eqtype" (hyp |- goal)

elim :: (MonadRule m) => Var -> Rule m Judgement
elim f = mkRule $ \hyp g -> 
    case Tl.lookupKey f hyp of
        Just p@(Pi _) -> do
            -- The hard part about this is that 'x:a -> b -> c -> d' should have
            -- '(((x _a) _b) _c)' as it's extract, and the way we handle substitutions WRT metavariables is bad
            (goals, _, extract, outputTy) <- elimArgs (Tl.empty, hyp, Var f) p
            -- This is where things get tricky. We want to be able to show that
            -- given a term of type 'd', we can prove our original goal
            x <- wildcard
            (mGoal, mHole) <- goal (hyp @> (x,outputTy) |- g)
            return (goals @> mGoal |>> subst x extract mHole)
            -- (aGoal, aHole) <- goal (hyp |- a)
            -- f' <- fresh $ string2Name ((name2String f) ++ "'")
            -- (bGoal, bHole) <- goal (hyp @> (f',b) |- g)
            -- let extract = (App (Var f) aHole)
            -- trace (show extract) return ()
            -- return (Tl.empty @> bGoal @> aGoal |> (App bHole aHole))
        Just t -> throwError $ ElimMismatch "fun/elim" t
        Nothing -> throwError $ UndefinedVariable f

    where
        elimArgs :: (MonadRule m) => (Telescope Extract Judgement, Telescope Term Term, Term) -> Term -> m (Telescope Extract Judgement, Telescope Term Term, Term, Term)
        elimArgs (goals, hyp, extract) (Pi bnd) = do
            -- This just recurses down the function type, adding goals and building up our extract term
            ((x, unembed -> a), b) <- unbind bnd
            -- ((x, unembed -> a), b) <- unbind bnd
            -- x' <- metavar x
            (aGoal, aHole) <- goal (hyp |- a)
            let extract' = (App extract aHole)
            elimArgs (goals @> aGoal, hyp @> (x,a), extract') b
        elimArgs (goals, hyp, extract) t = return (goals, hyp, extract, t)
